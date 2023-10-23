{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE BangPatterns #-}

module Symbolic.Sem where

import Data.Composition
import qualified Data.ByteArray as BA
import Text.Hex ( encodeHex )
import qualified Data.Text as Text
import Data.Function.Syntax
import Data.Bits hiding (And, Or, Xor)
import Prelude hiding (EQ, GT, LT)
import Concrete.Memory
import Env
import Data.Maybe (fromJust)
import Utils
import Numeric
import Ast

import Debug.Trace
import qualified Data.List as L
import Data.Either
import Symbolic.SMT
import Protolude.Functor
import GenericSem
import Data.Tuple.HT (mapSnd)
import Control.Monad.Extra (concatMapM, filterM)

data Extra = Extra { constraints :: [Expr], callDataSize :: Expr, returnDataSize :: Expr, varcount :: Integer, exprEnv :: [(String, Expr)]  }
    deriving Show

eof :: State -> Expr -> Expr
eof s e = foldl (\e (k,v) -> Let k v e) e s.extra.exprEnv

(#@) :: State -> Expr -> (State, Expr)
(#@) s e = 
    let v = "t" ++ show s.extra.varcount in
    (s{extra=s.extra{varcount = s.extra.varcount + 1, exprEnv = (v, e):s.extra.exprEnv}}, Var v)

data Balance = Balance
    { balances :: Expr, allowances :: Expr }
    deriving Show

type State  = GState  Extra Balance Expr Expr Expr
type Result = GResult Extra Balance Expr Expr Expr


(##) :: State -> Expr -> State
(##) s e = s{extra=s.extra{constraints=e:s.extra.constraints}}

extractInt :: Expr -> Integer
extractInt (LBitVec s n) = n
extractInt (LInt n) = n

filterFeasible :: ([Expr] -> IO Bool) -> [State] -> IO [State]
filterFeasible sat = filterM (\s -> sat . (:[]) . eof s $ ManyOp Land s.extra.constraints)

sem :: ([Expr] -> IO Bool) -> (Expr -> IO Expr) -> State -> IO [Result]
sem sat simp s0 =
    let b0 = s0.balance.balances in
    let (s1, b1) = s0 #@ (b0 @+ (word 0 +++ s0.caller, (b0 @! (word 0 +++ s0.caller)) `bvsub` s0.callValue)) in
    let (s2, b2) = s1 #@ (b1 @+ (word 0 +++ s1.id,     (b1 @! (word 0 +++ s1.id))     `bvadd` s1.callValue)) in
    sem_ sat simp $ s2{balance=s2.balance{balances=b2}} ## lnot ((b0 @! (word 0 +++ s0.caller)) `bvult` s0.callValue)

sem_ :: ([Expr] -> IO Bool) -> (Expr -> IO Expr) -> State -> IO [Result]
sem_ sat simp s = do
    stepped <- sem1 sat simp s
    let (results, states) = partitionEithers stepped
    results' <- concatMapM (sem_ sat simp) states
    return $ results ++ results'

semcall :: ([Expr] -> IO Bool) -> (Expr -> IO Expr) -> State -> Expr -> Expr -> Expr -> Expr -> Expr -> Expr -> Expr -> IO [Either Result State]
semcall sat simp s0 g a v ao as ro rs = do
    sro <- simp (eof s0 $ ro `bvmul` word 8) <&> extractInt
    srs <- simp (eof s0 $ rs `bvmul` word 8) <&> extractInt
    sao <- simp (eof s0 $ ao `bvmul` word 8) <&> extractInt
    let p0 = mextract (sao +   0)  32 s0.memory
    let p1 = mextract (sao +  32) 256 s0.memory
    let p2 = mextract (sao + 288) 256 s0.memory
    let p3 = mextract (sao + 544) 256 s0.memory
    let m0 = s0.memory
    let a0 = s0.balance.allowances
    let b0 = s0.balance.balances
    let (s1, b1) = s0 #@ (b0 @+ (word 0 +++ s0.id, (b0 @! (word 0 +++ s0.id)) `bvsub` v))
    let (s2, b2) = s1 #@ (b1 @+ (word 0 +++ a,     (b1 @! (word 0 +++ a))     `bvadd` v))
    Right <<$>> (filterFeasible sat [
            let !transferFId = LBitVec 32 . bytesToInteger $ funId "transfer(address,uint256)" in
            let (s3, b3) = s2 #@ (b2 @+ (a +++ s2.id, (b2 @! (a +++ s2.id)) `bvsub` p2)) in
            let (s4, b4) = s3 #@ (b3 @+ (a +++ p1,    (b3 @! (a +++ p1))    `bvadd` p2)) in
            let (s5, m1) = s4 #@ (mreplace sro (wextract 0 srs (word 1)) srs m0) in
            s5{stack=word 1:s5.stack, balance=s5.balance{balances=b4}, memory=m1, returned=word 1, extra=s5.extra{returnDataSize=word 32}}
                ## (as `eq` word 68)
                ## (p0 `eq` transferFId)
                ## (lnot ((b0 @! (a      +++ s0.id)) `bvult` p2))
                ## (lnot ((b0 @! (word 0 +++ s0.id)) `bvult` v))
        ,
            let transferFromFId = LBitVec 32 . bytesToInteger $ funId "transferFrom(address,address,uint256)" in
            let (s3, b3) = s2 #@ (b2 @+ (a +++ p1, (b2 @! (a +++ p1)) `bvsub` p3)) in
            let (s4, b4) = s3 #@ (b3 @+ (a +++ p2, (b3 @! (a +++ p2)) `bvadd` p3)) in
            let (s5, a1) = s4 #@ (a0 @+ (a +++ s4.id +++ p1, (a0 @! (a +++ s4.id +++ p1)) `bvsub` p3)) in
            let (s6, m1) = s5 #@ (mreplace sro (wextract 0 srs (word 1)) srs m0) in
            s6{stack=word 1:s6.stack, balance=s6.balance{balances=b4, allowances=a1}, memory=m1, returned=word 1, extra=s6.extra{returnDataSize=word 32}}
                ## (as `eq` word 100)
                ## (p0 `eq` transferFromFId)
                ## (lnot ((b0 @! (a +++ p1)) `bvult` p3))
                ## (lnot ((a0 @! (a +++ s0.id +++ p1)) `bvult` p3))
                ## (lnot ((b0 @! (word 0 +++ s0.id)) `bvult` v))
        ,
            let balanceOfFId = LBitVec 32 . bytesToInteger $ funId "balanceOf(address)" in
            let (s3, m1) = s2 #@ (mreplace sro (b2 @! (a +++ p1)) srs m0) in
            s3{stack=word 1:s3.stack, balance=s3.balance{balances=b2}, memory=m1, returned=(b2 @! (a +++ p1)), extra=s3.extra{returnDataSize=word 32}}
                ## (as `eq` word 36)
                ## (p0 `eq` balanceOfFId)
                ## (lnot ((b0 @! (word 0 +++ s0.id)) `bvult` v))
        ,
            s2{stack=word 1:s2.stack, balance=s2.balance{balances=b2}, extra=s2.extra{returnDataSize=word 0}}
                ## (as `eq` word 0)
                ## (lnot ((b0 @! (word 0 +++ s0.id)) `bvult` v))
        ,
            s2{stack=word 0:s2.stack}
                ## ((b0 @! (word 0 +++ s0.id)) `bvult` v)
        ])

sem1 :: ([Expr] -> IO Bool) -> (Expr -> IO Expr) -> State -> IO [Either Result State]
sem1 sat simp s = increasePC <<<$>>> case s.program `opAt` s.pc of
        STOP            -> return [Left (Returned (LBitVec 1 0, s))]
        ADD             -> return [sembop bvadd s]
        MUL             -> return [sembop bvmul s]
        SUB             -> return [sembop bvsub s]
        DIV             -> return [sembop bvudiv s]
        SDIV            -> return [sembop bvsdiv s]
        MOD             -> return [sembop bvurem s]
        SMOD            -> return [sembop bvsrem s]
        ADDMOD          -> return [semtop (TerOp AddMod) s]
        MULMOD          -> return [semtop (TerOp MulMod) s]
        LT              -> return [sembop (bool2bitvec .* bvult) s]
        GT              -> return [sembop (bool2bitvec .* bvugt) s]
        SLT             -> return [sembop (bool2bitvec .* bvslt) s]
        SGT             -> return [sembop (bool2bitvec .* bvsgt) s]
        EQ              -> return [sembop (bool2bitvec .* eq) s]
        ISZERO          -> return [semuop (bool2bitvec . eq (word 0)) s]
        AND             -> return [sembop bvand s]
        OR              -> return [sembop bvor s]
        XOR             -> return [sembop bvxor s]
        NOT             -> return [semuop bvnot s]
        BYTE            -> return [sembop (BinOp Byte) s]
        SHL             -> return [sembop (flip bvshl) s]
        SHR             -> return [sembop (flip bvlshr) s]
        SAR             -> return [sembop (flip bvashr) s]
        SHA3            -> return [pop2 s] >>>>= \(s, off, size) -> do
            soff <- simp (eof s $ off `bvmul` word 8) <&> extractInt
            ssize <- simp (eof s $ size `bvmul` word 8) <&> extractInt
            return [Right s{stack=(Var "hashfn" @! mextract soff ssize s.memory):s.stack}]
        ADDRESS         -> return [push s.id s]
        BALANCE         -> return [semuop ((s.balance.balances @!) . (word 0 +++)) s]
        SELFBALANCE     -> return [push (s.balance.balances @! (word 0 +++ s.id)) s]
        ORIGIN          -> return [push s.caller s]
        CALLER          -> return [push s.caller s]
        CALLVALUE       -> return [push s.callValue s]
        CALLDATASIZE    -> return [push s.extra.callDataSize s]
        CALLDATALOAD    -> return [pop s] >>>>= \(s, off) -> do
            soff <- simp (eof s $ off `bvmul` word 8) <&> extractInt
            return [Right s{stack=(mextract soff 256 s.callData):s.stack}]
        POP             -> return [pop_ s]
        JUMP            -> return [pop s >>= \(s, LBitVec _ loc) -> Right s{pc=fromIntegral loc-1}]
        JUMPI           -> return [pop2 s] >>>>= \(s, LBitVec _ loc, b) ->  Right <<$>> (filterFeasible sat
                                [ s                        ##      (b `eq` word 0)
                                , s{pc=fromIntegral loc-1} ## lnot (b `eq` word 0) ])
        JUMPDEST        -> return [Right s]
        DUP i           -> return [lookn (i-1) s >>= \(s, h) -> push h s]
        SWAP i          -> return [pop s >>= \(s, h) -> popn (i-1) s >>= \(s, m) -> pop s >>= \(s, t) -> pushn (t:m++[h]) s]
        SLOAD           -> return [semuop (s.storage @!) s]
        SSTORE          -> return [pop2 s >>= \(s, k, v) -> Right s{storage=s.storage @+ (k,v)}]
        MLOAD           -> return [pop s] >>>>= \(s, off) -> do
            soff <- simp (eof s $ off `bvmul` word 8) <&> extractInt
            return [Right s{stack=(mextract soff 256 s.memory):s.stack}]
        MSTORE          -> return [pop2 s] >>>>= \(s, off, val) -> do
            soff <- simp (eof s $ off `bvmul` word 8) <&> extractInt
            let (s1, m1) = s #@ mreplace soff val 256 s.memory
            return [Right s1{memory=m1}]
        PUSH size v     -> return [push (word v) s{pc=s.pc + size}]
        RETURN          -> return [pop2 s >>= \(s, off, size) -> Left (Returned (s.memory, s))]
        GAS             -> return [push (word 1) s]
        RETURNDATASIZE  -> return [push s.extra.returnDataSize s]
        -- RETURNDATACOPY  -> [pop3 s >>= \(s, doff, off, LBitVec _ 0) -> Right s]
        RETURNDATACOPY  -> return [pop3 s] >>>>= \(s, doff, off, size) -> do
            sdoff <- simp (eof s $ doff `bvmul` word 8) <&> extractInt
            sfullsize <- simp (eof s $ s.extra.returnDataSize `bvmul` word 8) <&> extractInt
            soff <- simp (eof s $ off `bvmul` word 8) <&> extractInt
            ssize <- simp (eof s $ size `bvmul` word 8) <&> extractInt
            let (s1, m1) = s #@ mreplace sdoff (extract sfullsize soff ssize s.returned) ssize s.memory
            return [Right s1{memory=m1}]
        MISSING         -> return [Left Reverted]
        INVALID         -> return [Left Reverted]
        REVERT          -> return [Left Reverted]
        CALL            -> return [popn 7 s] >>>>= \(s, [g,a,v,ao,as,ro,rs]) -> semcall sat simp s g a v ao as ro rs
        STATICCALL      -> return [popn 6 s] >>>>= \(s, [g,a,ao,as,ro,rs]) -> semcall sat simp s g a (word 0) ao as ro rs
        x               -> error (show x ++ " NOT IMPLEMENTED")
