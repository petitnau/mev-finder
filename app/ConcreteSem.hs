{-# LANGUAGE OverloadedRecordDot #-}

module ConcreteSem where

import Uint256
import Data.Composition
import qualified Data.ByteArray as BA
import Text.Hex ( encodeHex )
import qualified Data.Text as Text
import Data.Function.Syntax
import Data.Bits hiding (And, Or, Xor)
import Prelude hiding (EQ, GT, LT)
import Memory 
import Env
import Data.Maybe (fromJust)
import Utils (printMem)
import Numeric
import Ast

import Debug.Trace
import qualified Data.List as L

type Address = Uint256

data CallState = CallState
    { id :: Address
    , caller :: Address
    , callValue :: Uint256
    , callData :: BA.Bytes }
    deriving Show

data BlockInfo = BlockInfo
    { number :: Uint256
    , timestamp :: Uint256
    , balances :: Env Uint256 Uint256 }
    deriving Show

-- [Uint256]>
type Stack = [Uint256]

-- [Uint8]
type Memory = BA.Bytes

-- Uint256 -> Uint256
type Storage = Env Uint256 Uint256

data State = State
    { callState :: CallState
    , program :: Program 
    , pc :: Int
    , origin :: Address
    , block :: BlockInfo
    , stack :: Stack
    , memory :: Memory
    , storage :: Storage }

instance Show State where
    show s = 
        "callData = " ++ (printMem . Text.unpack . encodeHex . BA.pack . BA.unpack $ s.callState.callData) ++ "\n" ++
        "memory   = " ++ (printMem . Text.unpack . encodeHex . BA.pack . BA.unpack $ s.memory) ++ "\n" ++
        "stack    = " ++ "[" ++ (L.intercalate "," . map (\x -> "0x" ++ showHex x "") $ s.stack) ++ "]" ++ "\n" ++
        "block    = " ++ show s.block ++ "\n"

data Result 
    = Returned BA.Bytes 
    | Reverted
instance Show Result where
    show (Returned ba) = printMem . Text.unpack . encodeHex . BA.pack . BA.unpack $ ba
    show (Reverted) = "Reverted"

pop :: State -> Either Result State
pop s@State{stack = a:r} = Right s{stack=r, pc=s.pc+1}
pop _ = Left Reverted

push :: (Uint256) -> State -> Either Result State
push v s@State{stack = r} = Right s{stack=v:r, pc=s.pc+1}

semuop :: (Uint256 -> Uint256) -> State -> Either Result State
semuop op s@State{stack = a:r} = Right s{stack=op a:r, pc=s.pc+1}
semuop _ _ = Left Reverted

sembop :: (Uint256 -> Uint256 -> Uint256) -> State -> Either Result State
sembop op s@State{stack = a:b:r} = Right s{stack=op a b:r, pc=s.pc+1}
sembop _ _ = Left Reverted

semtop :: (Uint256 -> Uint256 -> Uint256 -> Uint256) -> State -> Either Result State
semtop op s@State{stack = a:b:c:r} = Right s{stack=op a b c:r, pc=s.pc+1}
semtop _ _ = Left Reverted

sem :: State -> Result
sem s = case traceShowId $ sem1 s of
        Right s -> sem s
        Left r -> r

semcall :: State -> Either Result State
semcall s@State{stack = g:a:v:ao:as:ro:rs:r} = 
    if Env.getDefault 0 s.block.balances s.callState.id < v then
        Right s{stack=0:r, pc=s.pc+1}
    else 
        let b0 = s.block.balances in
        let b1 = b0 `Env.bind` (s.callState.id, Env.getDefault 0 b0 s.callState.id - v) in
        let b2 = b1 `Env.bind` (a,              Env.getDefault 0 b1 a              + v) in
        Right s{
            stack=1:r, 
            pc=s.pc+1, 
            block=s.block{balances=b2}}

sem1 :: State -> Either Result State
sem1 s = case traceShowId $ s.program `opAt` s.pc of
        STOP            -> Left (Returned mempty)
        ADD             -> sembop (+) s
        MUL             -> sembop (*) s
        SUB             -> sembop (-) s
        DIV             -> sembop div s
        SDIV            -> sembop (/+) s
        MOD             -> sembop mod s
        SMOD            -> sembop (%+) s
        ADDMOD          -> semtop (\a b n -> (a + b) `mod` n) s
        MULMOD          -> semtop (\a b n -> (a * b) `mod` n) s
        EXP             -> sembop (^%) s
        LT              -> sembop (fromBool .* (<)) s
        GT              -> sembop (fromBool .* (>)) s
        SLT             -> sembop (fromBool .* (<+)) s
        SGT             -> sembop (fromBool .* flip (<+)) s
        EQ              -> sembop (fromBool .* (==)) s
        ISZERO          -> semuop (fromBool . (==) 0) s
        AND             -> sembop (.&.) s
        OR              -> sembop (.|.) s
        XOR             -> sembop xor s
        NOT             -> semuop complement s
        BYTE            -> sembop (\x i  -> (x `shift` fromIntegral (248 - i*8)) .&. 0xff) s
        SHL             -> sembop (flip (shiftL *. fromIntegral)) s
        SHR             -> sembop (flip (shiftR *. fromIntegral)) s
        SAR             -> error "SAR TODO"
        SHA3            -> error "SHA3 TODO"
        ADDRESS         -> push s.callState.id s
        BALANCE         -> semuop (getDefault 0 s.block.balances) s
        ORIGIN          -> push s.callState.id s
        CALLER          -> push s.callState.caller s
        CALLVALUE       -> push s.callState.callValue s
        CALLDATALOAD    -> semuop (getWord s.callState.callData . fromIntegral) s
        CALLDATASIZE    -> push (fromIntegral $ BA.length s.callState.callData) s
        TIMESTAMP       -> push (s.block.timestamp) s
        NUMBER          -> push (s.block.number) s
        SELFBALANCE     -> push (getDefault 0 s.block.balances s.callState.id) s
        POP             -> pop s
        MLOAD           -> semuop (getWord s.memory . fromIntegral) s
        MSTORE          -> case s.stack of off:val:r -> Right s{stack=r, memory=updateBytes s.memory (fromWord val) (fromIntegral off), pc=s.pc+1}; _  -> error "fuck"
        MSTORE8         -> case s.stack of off:val:r -> Right s{stack=r, memory=updateBytes s.memory (BA.pack [lowestByte val]) (fromIntegral off), pc=s.pc+1}; _  -> error "fuck"
        SLOAD           -> semuop (getDefault 0 s.storage) s
        SSTORE          -> case s.stack of key:val:r -> Right s{stack=r, storage=bind s.storage (key, val), pc=s.pc+1}; _  -> error "fuck"
        JUMP            -> case s.stack of loc:r -> Right s{stack=r, pc=fromIntegral loc}; _  -> error "fuck"
        JUMPI           -> case s.stack of loc:b:r -> if toBool b then Right s{stack=r, pc=fromIntegral loc} else Right s{stack=r, pc=s.pc+1}; _  -> error "fuck"
        PC              -> push (fromIntegral s.pc) s
        MSIZE           -> push (fromIntegral $ BA.length s.memory) s
        JUMPDEST        -> Right s{pc=s.pc+1}
        PUSH size v     -> push (fromIntegral v) s{pc=s.pc + size}
        DUP i           -> push (s.stack !! (i-1)) s
        SWAP i          -> 
            let (a,l1,b,l2) = (s.stack!!0, take (i-1) . drop 1 $ s.stack, s.stack!!i, drop (i+1) s.stack) in
                Right s{stack=b:l1 ++ a:l2, pc=s.pc+1}
        MISSING         -> Left Reverted
        INVALID         -> Left Reverted
        REVERT          -> Left Reverted
        RETURN          -> case s.stack of off:size:_ -> Left (Returned (takeExt (fromIntegral size) . BA.drop (fromIntegral off) $ s.memory)); _ -> error "fuck"
        RETURNDATASIZE  -> push 0 s
        GAS             -> push 1 s
        CALL            -> semcall s
        x               -> error (show x ++ " NOT IMPLEMENTED")
