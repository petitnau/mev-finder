{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE ScopedTypeVariables #-}

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
import Utils (printMem, (>>>=))
import Numeric
import Ast

import Debug.Trace
import qualified Data.List as L
import Data.Either
import Symbolic.SMT
import Protolude.Functor
import GenericSem
import Data.Tuple.HT (mapSnd)

data Extra = Extra { constraints :: [Expr], callDataSize :: Expr }
    deriving Show

type State  = GState  Extra Expr Expr Expr Expr
type Result = GResult Extra Expr Expr Expr Expr


(##) :: State -> Expr -> State
(##) s e = s{extra=s.extra{constraints=e:s.extra.constraints}}

sem :: State -> [Result]
sem s =
    let b0 = s.balances in
    let b1 = b0 @+ (s.caller, (b0 @! s.caller) `bvsub` s.callValue) in
    let b2 = b1 @+ (s.id,     (b1 @! s.id)     `bvadd` s.callValue) in
    uncurry (++) . mapSnd concat . partitionEithers . fmap (fmap sem_) $ 
        [ Right $ s{balances=b2} ## lnot ((b0 @! s.caller) `bvult` s.callValue)
        , Left Reverted ]

sem_ :: State -> [Result]
sem_ s =
    let (results, states) = partitionEithers (sem1 s) in
    results ++ concatMap sem_ states

semcall :: State -> [Either Result State]
semcall s@State{stack = g:a:v:ao:as:ro:rs:r} =
    let b0 = s.balances in
    let b1 = b0 @+ (s.id, (b0 @! s.id) `bvsub` v) in
    let b2 = b1 @+ (a,    (b1 @! a)    `bvadd` v) in
    Right <$>
        [ s{stack=word 1:r, balances=b2} ## lnot ((b0 @! s.id) `bvult` v)
        , s{stack=word 0:r}              ##      ((b0 @! s.id) `bvult` v) ]

sem1 :: State -> [Either Result State]
sem1 s = increasePC <<$>> case s.program `opAt` s.pc of
        STOP            -> [Left (Returned (LBitVec 1 0, s))]
        ADD             -> [sembop bvadd s]
        MUL             -> [sembop bvmul s]
        SUB             -> [sembop bvsub s]
        DIV             -> [sembop bvudiv s]
        SDIV            -> [sembop bvsdiv s]
        MOD             -> [sembop bvurem s]
        SMOD            -> [sembop bvsrem s]
        ADDMOD          -> [semtop (TerOp AddMod) s]
        MULMOD          -> [semtop (TerOp MulMod) s]
        EXP             -> [sembop (BinOp Exp) s]
        LT              -> [sembop (bool2bitvec .* bvult) s]
        GT              -> [sembop (bool2bitvec .* bvugt) s]
        SLT             -> [sembop (bool2bitvec .* bvslt) s]
        SGT             -> [sembop (bool2bitvec .* bvsgt) s]
        EQ              -> [sembop (bool2bitvec .* eq) s]
        ISZERO          -> [semuop (bool2bitvec . eq (word 0)) s]
        AND             -> [sembop bvand s]
        OR              -> [sembop bvor s]
        XOR             -> [sembop bvxor s]
        NOT             -> [semuop bvnot s]
        BYTE            -> [sembop (BinOp Byte) s]
        SHL             -> [sembop (flip bvshl) s]
        SHR             -> [sembop (flip bvlshr) s]
        SAR             -> [sembop (flip bvashr) s]
        SHA3            -> [sembop (\off size -> UnOp Hash (extract off size s.memory )) s]
        ADDRESS         -> [push s.id s]
        BALANCE         -> [semuop (s.balances @!) s]
        SELFBALANCE     -> [push (s.balances @! s.id) s]
        ORIGIN          -> [push s.caller s]
        CALLER          -> [push s.caller s]
        CALLVALUE       -> [push s.callValue s]
        CALLDATASIZE    -> [push s.extra.callDataSize s]
        CALLDATALOAD    -> [semuop (\x -> extract (x `bvmul` word 8) (word 256) s.callData) s]
        POP             -> [pop_ s]
        JUMP            -> [pop s >>= \(s, LBitVec _ loc) -> Right s{pc=fromIntegral loc-1}]
        JUMPI           -> [pop2 s] >>>= \(s, LBitVec _ loc, b) -> Right <$>
                                [ s                        ##      (b `eq` word 0)
                                , s{pc=fromIntegral loc-1} ## lnot (b `eq` word 0) ]
        JUMPDEST        -> [Right s]
        DUP i           -> [lookn (i-1) s >>= \(s, h) -> push h s]
        SWAP i          -> [pop s >>= \(s, h) -> popn (i-1) s >>= \(s, m) -> pop s >>= \(s, t) -> pushn (t:m++[h]) s]
        SLOAD           -> [semuop (s.storage @!) s]
        SSTORE          -> [pop2 s >>= \(s, k, v) -> Right s{storage=s.storage @+ (k,v)}]
        MLOAD           -> [semuop (\x -> extract (x `bvmul` word 8) (word 256) s.memory) s]
        MSTORE          -> [pop2 s >>= \(s, off, val) -> Right s{memory=replace off val (word 256) s.memory}]
        PUSH size v     -> [push (word v) s{pc=s.pc + size}]
        RETURN          -> [pop2 s >>= \(s, off, size) -> Left (Returned (s.memory, s))]
        GAS             -> [push (word 1) s]
        RETURNDATASIZE  -> [push (word 0) s]
        RETURNDATACOPY  -> [pop3 s >>= \(s, doff, off, LBitVec _ 0) -> Right s]
        MISSING         -> [Left Reverted]
        INVALID         -> [Left Reverted]
        REVERT          -> [Left Reverted]
        CALL            -> semcall s
        x               -> error (show x ++ " NOT IMPLEMENTED" ++ show s.stack)
