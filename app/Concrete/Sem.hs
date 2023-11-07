{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE BangPatterns #-}


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
import Concrete.IERC20
import Data.Either.Extra
import Control.Monad (when)

data Balance = Balance
    { eth :: Env Uint256 Uint256
    , tok :: Env Uint256 ERC20 }
    deriving Show

type State  = GState  () Balance Uint256 BA.Bytes (Env Uint256 Uint256)
type Result = GResult () Balance Uint256 BA.Bytes (Env Uint256 Uint256)

prettyState :: State -> String
prettyState s =
    "-- CALLDATA --" ++ (printMem . Text.unpack . encodeHex . BA.pack . BA.unpack $ s.callData) ++ "\n" ++
    "--  MEMORY  --" ++ (printMem . Text.unpack . encodeHex . BA.pack . BA.unpack $ s.memory) ++ "\n" ++
    "--   STACK  --" ++ "\n[" ++ (L.intercalate "," . map (\x -> "0x" ++ showHex (toWord256 x) "") $ s.stack) ++ "]" ++ "\n" ++
    "-- STORAGE  --" ++ "\n" ++ show s.storage ++ "\n" ++
    "--  RETURN  --" ++ (printMem . Text.unpack . encodeHex . BA.pack . BA.unpack $ s.returned) ++ "\n" ++
    "--   ETH    --" ++ "\n" ++ show s.balances.eth ++ "\n" ++
    L.intercalate "\n" (zipWith (\i a -> "--  TOK " ++ show i ++ "   --" ++ "\n" ++ show (fromJust $ Env.get s.balances.tok a)) [1..] (Env.domain s.balances.tok))

prettyResult :: Result -> String
prettyResult Reverted = "Reverted"
prettyResult (Returned (ba, s)) =
    prettyState s ++ "\n" ++
    "--  RESULT  --" ++ (printMem . Text.unpack . encodeHex . BA.pack . BA.unpack $ ba)

-----------------

sem :: State -> Result
sem s =
    let b0 = s.balances.eth in
    let b1 = b0 `Env.bind` (s.id,     bvadd (Env.getDefault 0 b0 s.id)     s.callValue) in
    let b2 = b1 `Env.bind` (s.caller, bvsub (Env.getDefault 0 b1 s.caller) s.callValue) in
    if Env.getDefault 0 b0 s.caller < s.callValue then
        Reverted
    else
        sem_ s{balances=s.balances{eth=b2}}

sem_ :: State -> Result
sem_ s = case sem1 s of
        Right s -> sem_ $ traceWith prettyState s
        Left r -> r

execCall :: State -> Uint256 -> BA.Bytes -> Maybe (State, BA.Bytes)
execCall s addr mem =
    if mem == mempty then
        return (s, mempty)
    else do
        erc20 <- Env.get s.balances.tok addr
        let fun = Memory.takeExt 4 mem
        (erc20, ret) <-
            if fun == funId "transfer(address,uint256)" then do
                let to  = Memory.getUint256 mem 4
                let amt = Memory.getUint256 mem 36
                (erc20, res) <- transfer s.id to amt erc20
                return (erc20, Memory.fromUint256 res)
            else if fun == funId "transferFrom(address,address,uint256)" then do
                let from = Memory.getUint256 mem 4
                let to   = Memory.getUint256 mem 36
                let amt  = Memory.getUint256 mem 68
                (erc20, res) <- traceShow ("HERE", s.id, from, to, amt, erc20) $ transferFrom s.id from to amt erc20
                return (erc20, Memory.fromUint256 res)
            else if fun == funId "balanceOf(address)" then do
                let addr = Memory.getUint256 mem 4
                (erc20, res) <- balanceOf addr erc20
                return (erc20, Memory.fromUint256 res)
            else error (show fun)
        return (s{balances=s.balances{tok=s.balances.tok `Env.bind` (addr, erc20)}}, ret)

call :: State -> Uint256 -> Uint256 -> Uint256 -> Uint256 -> Uint256 -> Uint256 -> Uint256 -> Either Result State
call s g a v ao as ro rs =
    let subctx = do
            when (Env.getDefault 0 s.balances.eth s.id < v) (Left Reverted)
            (s, ret) <- maybeToEither Reverted $ execCall s a (Memory.takeExt (touInt as) . BA.drop (touInt ao) $ s.memory)
            let s1 = s{memory=Memory.updateBytes s.memory ret (touInt ro), returned=ret}
            let b0 = s1.balances
            let b1 = b0{eth=b0.eth `Env.bind` (s.id, bvsub (Env.getDefault 0 b0.eth s.id) v)}
            let b2 = b1{eth=b1.eth `Env.bind` (a,    bvadd (Env.getDefault 0 b1.eth a)    v)}
            return (s1{balances=b2})
    in
    case subctx of
        Left _  -> push 0 s
        Right s -> push 1 s


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
        BALANCE         -> semuop (getDefault 0 s.balances.eth) s
        ORIGIN          -> push s.id s
        CALLER          -> push s.caller s
        CALLVALUE       -> push s.callValue s
        CALLDATALOAD    -> semuop (getUint256 s.callData . touInt) s
        CALLDATASIZE    -> push (fromIntegral . BA.length $ s.callData) s
        TIMESTAMP       -> push s.timestamp s
        NUMBER          -> push s.number s
        SELFBALANCE     -> push (getDefault 0 s.balances.eth s.id) s
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
        RETURNDATACOPY  -> pop3 s >>= \(s, doff, off, size) ->
            Right s{memory=updateBytes s.memory (takeExt (touInt size) . BA.drop (touInt off) $ s.returned) (touInt doff)}
        RETURNDATASIZE  -> push (fromIntegral . BA.length $ s.returned) s
        GAS             -> push 1 s
        CALL            -> popn 7 s >>= \(s, [g,a,v,ao,as,ro,rs]) -> call s g a v ao as ro rs
        STATICCALL      -> popn 6 s >>= \(s, [g,a,ao,as,ro,rs]) -> call s g a 0 ao as ro rs
        x               -> error (show x ++ " NOT IMPLEMENTED")
