{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE TupleSections #-}


module Concrete.Sem where

import Concrete.Utils
import Data.Composition
import qualified Data.ByteArray as BA
import Text.Hex ( encodeHex )
import qualified Data.Text as Text
import Data.Function.Syntax
import Data.Bits hiding (And, Or, Xor)
import Prelude hiding (EQ, GT, LT)
import Concrete.Memory as Memory
import Concrete.Uint256
import Env
import Data.Maybe (fromJust)
import Utils
import Numeric
import Ast
import Debug.Trace
import qualified Data.List as L
import GenericSem
import Data.Either.Extra
import Control.Monad (when)
import Symbolic.SMT (Expr(..), eq, (@!))

data Balance = Balance
    { balances :: Env (Uint256, Uint256) Uint256, allowances :: Env (Uint256, Uint256, Uint256) Uint256 }
    deriving Show

data Extra = Extra { constraints :: [Expr] }
    deriving Show

(##) :: State -> Expr -> State
(##) s e = s{extra=s.extra{constraints=e:s.extra.constraints}}

type State  = GState  Extra Balance Uint256 BA.Bytes (Env Uint256 Uint256)
type Result = GResult Extra Balance Uint256 BA.Bytes (Env Uint256 Uint256)

data CoreState = CoreState 
    { coreStorage :: Env Uint256 Uint256
    , coreBalance :: Env (Uint256, Uint256) Uint256 
    , coreAllowances :: Env (Uint256, Uint256, Uint256) Uint256 }
    deriving (Eq, Ord, Show)

prettyState :: State -> String
prettyState s =
    "-- CALLDATA --" ++ (printMem . Text.unpack . encodeHex . BA.pack . BA.unpack $ s.callData) ++ "\n" ++
    "--  MEMORY  --" ++ (printMem . Text.unpack . encodeHex . BA.pack . BA.unpack $ s.memory) ++ "\n" ++
    "--   STACK  --" ++ "\n[" ++ (L.intercalate "," . map (\x -> "0x" ++ showHex (toWord256 x) "") $ s.stack) ++ "]" ++ "\n" ++
    "-- STORAGE  --" ++ "\n" ++ show s.storage ++ "\n" ++
    "--  RETURN  --" ++ (printMem . Text.unpack . encodeHex . BA.pack . BA.unpack $ s.returned) ++ "\n" ++
    "--  BALANCE --" ++ "\n" ++ show s.balance.balances ++ "\n" ++
    "-- ALLOWANCE--" ++ "\n" ++ show s.balance.allowances ++ "\n" ++
    "-- CONSTRAINTS --" ++ "\n" ++ show s.extra ++ "\n"

prettyResult :: Result -> String
prettyResult Reverted = "Reverted"
prettyResult (Returned (ba, s)) =
    prettyState s ++ "\n" ++
    "--  RESULT  --" ++ (printMem . Text.unpack . encodeHex . BA.pack . BA.unpack $ ba)


transfer :: Uint256 -> Uint256 -> Uint256 -> Uint256 -> State -> Maybe (State, Uint256)
transfer tok from to amt s = do
    when (Env.getDefault 0 s.balance.balances (tok, from) < amt) Nothing
    let b0 = s.balance
    let b1 = b0{balances=b0.balances `Env.bind` ((tok, from), Env.getDefault 0 b0.balances (tok, from) `bvsub` amt)}
    let b2 = b1{balances=b1.balances `Env.bind` ((tok, to),   Env.getDefault 0 b1.balances (tok, to)   `bvadd` amt)}
    return (s{balance=b2}, 1)

transferFrom :: Uint256 -> Uint256 -> Uint256 -> Uint256 -> Uint256 -> State -> Maybe (State, Uint256)
transferFrom tok auth from to amt s = do
    when (Env.getDefault 0 s.balance.allowances (tok, auth, from) < amt) Nothing
    (s, _) <- transfer tok from to amt s
    let b0 = s.balance
    let b1 = b0{allowances=b0.allowances `Env.bind` ((tok, auth, from), Env.getDefault 0 b0.allowances (tok, auth, from) `bvsub` amt)}
    return (s{balance=b1}, 1)

balanceOf :: Uint256 -> Uint256 -> State -> Maybe (State, Uint256)
balanceOf tok acc s = return (s, Env.getDefault 0 s.balance.balances (tok, acc))

-----------------

sem :: State -> Result
sem s =
    let b0 = s.balance in
    let b1 = b0{balances=b0.balances `Env.bind` ((0, s.id),     bvadd (Env.getDefault 0 b0.balances (0, s.id))     s.callValue)} in
    let b2 = b1{balances=b1.balances `Env.bind` ((0, s.caller), bvsub (Env.getDefault 0 b1.balances (0, s.caller)) s.callValue)} in
    if Env.getDefault 0 s.balance.balances (0, s.caller) < s.callValue then
        Reverted
    else
        sem_ s{balance=b2}

sem_ :: State -> Result
sem_ s = case sem1 s of
        Right s -> sem_ (s)
        Left r -> r

execCall :: State -> Uint256 -> BA.Bytes -> Maybe (State, BA.Bytes)
execCall s addr mem =
    if mem == mempty then
        return (s, mempty)
    else do
        let fun = Memory.takeExt 4 mem
        if fun == funId "transfer(address,uint256)" then do
            let to  = Memory.getUint256 mem 4
            let amt = Memory.getUint256 mem 36
            (s, res) <- transfer addr s.id to amt s
            return (s, Memory.fromUint256 res)
        else if fun == funId "transferFrom(address,address,uint256)" then do
            let from = Memory.getUint256 mem 4
            let to   = Memory.getUint256 mem 36
            let amt  = Memory.getUint256 mem 68
            (s, res) <- transferFrom addr s.id from to amt s
            return (s, Memory.fromUint256 res)
        else if fun == funId "balanceOf(address)" then do
            let from = Memory.getUint256 mem 4
            (s, res) <- balanceOf addr from s
            return (s, Memory.fromUint256 res)
        else error (show fun)

call :: State -> Uint256 -> Uint256 -> Uint256 -> Uint256 -> Uint256 -> Uint256 -> Uint256 -> Either Result State
call s g a v ao as ro rs =
    let subctx = do
            when (Env.getDefault 0 s.balance.balances (0, s.id) < v) (trace (prettyState s) $ Left Reverted)
            (s, ret) <- maybeToEither Reverted $ execCall s a (Memory.takeExt (touInt as) . BA.drop (touInt ao) $ s.memory)
            let s1 = s{memory=Memory.updateBytes s.memory ret (touInt ro), returned=ret}
            let b0 = s1.balance
            let b1 = b0{balances=b0.balances `Env.bind` ((0, s.id), bvsub (Env.getDefault 0 b0.balances (0, s.id)) v)}
            let b2 = b1{balances=b1.balances `Env.bind` ((0, a),    bvadd (Env.getDefault 0 b1.balances (0, a))    v)}
            return (s1{balance=b2})
    in
    case subctx of
        Left _  -> push 0 s
        Right s -> push 1 s


sem1 :: State -> Either Result State
sem1 s = increasePC <$> case s.program `opAt` s.pc of
        STOP            -> Left (Returned (mempty, s))
        ADD             -> sembop bvadd s
        MUL             -> sembop bvmul s
        SUB             -> sembop bvsub s
        DIV             -> sembop bvuquot s
        SDIV            -> sembop bvsquot s
        MOD             -> sembop bvurem s
        SMOD            -> sembop bvsrem s
        ADDMOD          -> semtop (\a b n -> bvurem (bvadd a b) n) s
        MULMOD          -> semtop (\a b n -> bvurem (bvmul a b) n) s
        EXP             -> error "EXP TODO"
        LT              -> sembop (fromBool .* bvult) s
        GT              -> sembop (fromBool .* bvugt) s
        SLT             -> sembop (fromBool .* bvslt) s
        SGT             -> sembop (fromBool .* bvsgt) s
        EQ              -> sembop (fromBool .* (==)) s
        ISZERO          -> semuop (fromBool . (==) 0) s
        AND             -> sembop bvand s
        OR              -> sembop bvor s
        XOR             -> sembop bvxor s
        NOT             -> semuop bvcomplement s
        BYTE            -> sembop byteAt s
        SHL             -> sembop (flip bvshl) s
        SHR             -> sembop (flip bvlshr) s
        SAR             -> sembop (flip bvashr) s
        SHA3            -> pop2 s >>= \(s, off, sz) -> let (k, v) = hashMem s.memory off sz in push v (s ## eq (Var "hashfn" @! (LBitVec 1024 (touInteger k))) (LBitVec 256 (touInteger v)))
        ADDRESS         -> push s.id s
        BALANCE         -> semuop (Env.getDefault 0 s.balance.balances . (0,)) s
        ORIGIN          -> push s.id s
        CALLER          -> push s.caller s
        CALLVALUE       -> push s.callValue s
        CALLDATALOAD    -> semuop (getUint256 s.callData . touInt) s
        CALLDATASIZE    -> push (fromIntegral . BA.length $ s.callData) s
        TIMESTAMP       -> push s.timestamp s
        NUMBER          -> push s.number s
        SELFBALANCE     -> push (Env.getDefault 0 s.balance.balances (0,s.id)) s
        POP             -> pop_ s
        MLOAD           -> semuop (getUint256 s.memory . touInt ) s
        MSTORE          -> pop2 s >>= \(s, off, val) -> Right s{memory=updateBytes s.memory (fromUint256 val) (touInt off)}
        MSTORE8         -> pop2 s >>= \(s, off, val) -> Right s{memory=updateBytes s.memory (BA.pack [lbyte val]) (touInt off)}
        SLOAD           -> semuop (getDefault 0 s.storage) s
        SSTORE          -> pop2 s >>= \(s, key, val) -> Right s{storage=bind s.storage (key, val)}
        JUMP            -> pop  s >>= \(s, loc) -> Right s{pc=touInt loc - 1}
        JUMPI           -> pop2 s >>= \(s, loc, b) -> if toBool b then Right s{pc=touInt loc - 1} else Right s
        PC              -> push (fromIntegral s.pc) s
        MSIZE           -> push (fromIntegral $ BA.length s.memory) s
        JUMPDEST        -> Right s
        PUSH size v     -> push (fromInteger v) s{pc=s.pc + size}
        DUP i           -> lookn (i-1) s >>= \(s, h) -> push h s
        SWAP i          -> pop s >>= (\(s, h) -> popn (i-1) s >>= \(s, m) -> pop s >>= \(s, t) -> pushn (t:m++[h]) s)
        MISSING         -> trace (prettyState s) $ Left Reverted
        INVALID         -> trace (prettyState s) $ Left Reverted
        REVERT          -> trace (prettyState s) $ Left Reverted
        RETURN          -> pop2 s >>= \(s, off, size) -> Left (Returned (takeExt (touInt size) . BA.drop (touInt off) $ s.memory, s))
        RETURNDATACOPY  -> pop3 s >>= \(s, doff, off, size) ->
            Right s{memory=updateBytes s.memory (takeExt (touInt size) . BA.drop (touInt off) $ s.returned) (touInt doff)}
        RETURNDATASIZE  -> push (fromIntegral . BA.length $ s.returned) s
        GAS             -> push 1 s
        CALL            -> popn 7 s >>= \(s, [g,a,v,ao,as,ro,rs]) -> call s g a v ao as ro rs
        STATICCALL      -> popn 6 s >>= \(s, [g,a,ao,as,ro,rs]) -> call s g a 0 ao as ro rs
        x               -> error (show x ++ " NOT IMPLEMENTED")
