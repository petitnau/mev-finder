{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE DataKinds #-}

module ConcreteSem where

import Data.Composition
import qualified Data.ByteArray as BA
import Text.Hex ( encodeHex )
import qualified Data.Text as Text
import Data.Function.Syntax
import Data.Bits hiding (And, Or, Xor)
import Prelude hiding (EQ, GT, LT)
import Memory
import Uint256
import Env
import Data.Maybe (fromJust)
import Utils (printMem)
import Numeric
import Ast
import Debug.Trace
import qualified Data.List as L
import GenericSem

type State  = GState  () (Env Uint256 Uint256) Uint256 BA.Bytes (Env Uint256 Uint256)
type Result = GResult () (Env Uint256 Uint256) Uint256 BA.Bytes (Env Uint256 Uint256)

prettyState :: State -> String
prettyState s = 
    "-- CALLDATA --" ++ (printMem . Text.unpack . encodeHex . BA.pack . BA.unpack $ s.callData) ++ "\n" ++
    "--  MEMORY  --" ++ (printMem . Text.unpack . encodeHex . BA.pack . BA.unpack $ s.memory) ++ "\n" ++
    "--   STACK  --" ++ "\n[" ++ (L.intercalate "," . map (\x -> "0x" ++ showHex (toWord256 x) "") $ s.stack) ++ "]" ++ "\n" ++
    "-- STORAGE  --" ++ "\n" ++ show s.storage ++ "\n" ++
    "-- BALANCE  --" ++ "\n" ++ show s.balances ++ "\n"

prettyResult :: Result -> String
prettyResult (Returned (ba, s)) = 
    prettyState s ++ "\n" ++
    "--  RESULT  --" ++ (printMem . Text.unpack . encodeHex . BA.pack . BA.unpack $ ba)
prettyResult  Reverted          = "Reverted"

-----------------

sem :: State -> Result
sem s =
    let b0 = s.balances in
    let b1 = b0 `Env.bind` (s.id,     bvadd (Env.getDefault 0 b0 s.id)     s.callValue) in
    let b2 = b1 `Env.bind` (s.caller, bvsub (Env.getDefault 0 b1 s.caller) s.callValue) in
    if Env.getDefault 0 s.balances s.caller < s.callValue then
        Reverted
    else
        sem_ s{balances=b2}

sem_ :: State -> Result
sem_ s = case sem1 s of
        Right s -> sem_ s
        Left r -> r

semcall :: State -> Either Result State
semcall s@State{stack = g:a:v:ao:as:ro:rs:r} =
    let b0 = s.balances in
    let b1 = b0 `Env.bind` (s.id, bvsub (Env.getDefault 0 b0 s.id) v) in
    let b2 = b1 `Env.bind` (a,    bvadd (Env.getDefault 0 b1 a)    v) in
    if Env.getDefault 0 s.balances s.id < v then
        Right s{stack=0:r}
    else
        Right s{stack=1:r, balances=b2}

sem1 :: State -> Either Result State
sem1 s = increasePC <$> case traceShowId $ s.program `opAt` traceShowId s.pc of
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
        SHL             -> sembop (flip (bvshl *. toNatural)) s
        SHR             -> sembop (flip (bvlshr *. toNatural)) s
        SAR             -> sembop (flip (bvashr *. toNatural)) s
        SHA3            -> error "SHA3 TODO"
        ADDRESS         -> push s.id s
        BALANCE         -> semuop (getDefault 0 s.balances) s
        ORIGIN          -> push s.id s
        CALLER          -> push s.caller s
        CALLVALUE       -> push s.callValue s
        CALLDATALOAD    -> semuop (getUint256 s.callData . touInt) s
        CALLDATASIZE    -> push (fromIntegral . BA.length $ s.callData) s
        TIMESTAMP       -> push s.timestamp s
        NUMBER          -> push s.number s
        SELFBALANCE     -> push (getDefault 0 s.balances s.id) s
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
        MISSING         -> Left Reverted
        INVALID         -> Left Reverted
        REVERT          -> Left Reverted
        RETURN          -> pop2 s >>= \(s, off, size) -> Left (Returned (takeExt (touInt size) . BA.drop (touInt off) $ s.memory, s))
        RETURNDATASIZE  -> push 0 s
        GAS             -> push 1 s
        CALL            -> semcall s
        x               -> error (show x ++ " NOT IMPLEMENTED")
