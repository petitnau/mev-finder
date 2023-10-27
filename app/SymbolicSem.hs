{-# LANGUAGE OverloadedRecordDot #-}

module SymbolicSem where

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
import Data.Either

data UnOp
    = SignExtend | Sha3
    | Balance | CallDataAt
    | IsZero
    | Not
    deriving (Eq, Ord, Show)
data BinOp
    = Add | Mul | Sub | Div | SDiv | Mod | SMod | Exp | Byte | Shl | Shr | Sar
    | Lt | Gt | SLt | SGt | Eq
    | And | Or | Xor
    deriving (Eq, Ord, Show)
data TerOp
    = AddMod | MulMod
    deriving (Eq, Ord, Show)
data Const
    = Address | Origin | Caller | CallValue | CallDataSize | CodeSize | GasPrice | ExtCodeSize | ReturnDataSize | ExtCodeHash | BlockHash | Coinbase | Timestamp | Number | Difficulty | GasLimit | ChainId | SelfBalance | BaseFee | MSize | Gas
    deriving (Eq, Ord, Show)

data Expr
    = Const Const
    | UnOp  UnOp  Expr
    | BinOp BinOp Expr Expr
    | TerOp TerOp Expr Expr Expr
    | SVar String
    | Literal Integer
    | Select Expr Expr
    | Store Expr Expr Expr
    | EmptyStore
    deriving (Eq, Ord, Show)

data BlockInfo = BlockInfo
    { balances :: Expr }
    deriving Show

-- [Uint256]>
type Stack = [Expr]

-- [Uint8]
type Memory = Expr

-- Uint256 -> Uint256
type Storage = Expr

data State = State
    { program :: Program
    , pc :: Int
    , block :: BlockInfo
    , stack :: Stack
    , memory :: Memory
    , storage :: Storage
    , constraints :: [Expr] }
    deriving Show

data Result
    = Returned (Expr, Expr)
    | Reverted
    deriving Show

pop :: State -> Either Result State
pop s@State{stack = a:r} = Right s{stack=r, pc=s.pc+1}
pop _ = Left Reverted

push :: (Expr) -> State -> Either Result State
push v s@State{stack = r} = Right s{stack=v:r, pc=s.pc+1}

semuop :: (Expr -> Expr) -> State -> Either Result State
semuop op s@State{stack = a:r} = Right s{stack=op a:r, pc=s.pc+1}
semuop _ _ = Left Reverted

sembop :: (Expr -> Expr -> Expr) -> State -> Either Result State
sembop op s@State{stack = a:b:r} = Right s{stack=op a b:r, pc=s.pc+1}
sembop _ _ = Left Reverted

semtop :: (Expr -> Expr -> Expr -> Expr) -> State -> Either Result State
semtop op s@State{stack = a:b:c:r} = Right s{stack=op a b c:r, pc=s.pc+1}
semtop _ _ = Left Reverted

sem :: State -> [Result]
sem s =
    let (results, states) = partitionEithers (traceShowId $ sem1 s) in
    results ++ concatMap sem states

semcall :: State -> [Either Result State]
semcall s@State{stack = g:a:v:ao:as:ro:rs:r} =
    [ Right s{stack=Literal 0:r, pc=s.pc+1, constraints=BinOp Lt (Select s.block.balances (Const Address)) v:s.constraints}
    , let b0 = s.block.balances in
      let b1 = Store b0 (Const Address) (BinOp Sub (Select b0 (Const Address)) v) in
      let b2 = Store b1 a               (BinOp Add (Select b1 a              ) v) in
      Right s { stack=Literal 1:r
              , pc=s.pc+1
              , block=s.block{balances=b2}
              , constraints=UnOp Not (BinOp Lt (Select b0 (Const Address)) v):s.constraints} ]

sem1 :: State -> [Either Result State]
sem1 s = case traceShowId $ s.program `opAt` s.pc of
        STOP            -> [Left (Returned (Literal 0, s.block.balances))]
        ADD             -> [sembop (BinOp Add) s]
        MUL             -> [sembop (BinOp Mul) s]
        SUB             -> [sembop (BinOp Sub) s]
        DIV             -> [sembop (BinOp Div) s]
        SDIV            -> [sembop (BinOp SDiv) s]
        MOD             -> [sembop (BinOp Mod) s]
        SMOD            -> [sembop (BinOp SMod) s]
        ADDMOD          -> [semtop (TerOp AddMod) s]
        MULMOD          -> [semtop (TerOp MulMod) s]
        EXP             -> [sembop (BinOp Exp) s]
        LT              -> [sembop (BinOp Lt) s]
        GT              -> [sembop (BinOp Gt) s]
        SLT             -> [sembop (BinOp SLt) s]
        SGT             -> [sembop (BinOp SGt) s]
        EQ              -> [sembop (BinOp Eq) s]
        ISZERO          -> [semuop (UnOp  IsZero) s]
        AND             -> [sembop (BinOp And) s]
        OR              -> [sembop (BinOp Or) s]
        XOR             -> [sembop (BinOp Xor) s]
        NOT             -> [semuop (UnOp  Not) s]
        BYTE            -> [sembop (BinOp Byte) s]
        SHL             -> [sembop (BinOp Shl) s]
        SHR             -> [sembop (BinOp Shr) s]
        ADDRESS         -> [push (Const Address) s]
        BALANCE         -> [semuop (UnOp Balance) s]
        SELFBALANCE     -> [push (UnOp Balance (Const Address)) s]
        ORIGIN          -> [push (Const Origin) s]
        CALLER          -> [push (Const Caller) s]
        CALLVALUE       -> [push (Const CallValue) s]
        CALLDATASIZE    -> [push (Const CallDataSize) s]
        CALLDATALOAD    -> [semuop (UnOp CallDataAt) s]

        POP             -> [pop s]
        JUMP            -> case s.stack of (Literal loc):r -> [Right s{stack=r, pc=fromIntegral loc}]; _  -> error "fuck"
        JUMPI           -> case s.stack of
                                (Literal loc):b:r ->
                                    [ Right s{stack=r, pc=fromIntegral loc, constraints=b:s.constraints }
                                    , Right s{stack=r, pc=s.pc+1, constraints=UnOp Not b:s.constraints }]
                                ; _  -> error "fuck"
        JUMPDEST        -> [Right s{pc=s.pc+1}]
        DUP i           -> [push (s.stack !! (i-1)) s]
        SWAP i          ->
            let (a,l1,b,l2) = (s.stack!!0, take (i-1) . drop 1 $ s.stack, s.stack!!i, drop (i+1) s.stack) in
                [Right s{stack=b:l1 ++ a:l2, pc=s.pc+1}]

        MLOAD           -> [semuop (Select s.memory) s]
        MSTORE          -> case s.stack of off:val:r -> [Right s{stack=r, memory=Store s.memory off val, pc=s.pc+1}]; _  -> error "fuck"
        PUSH size v     -> [push (Literal v) s{pc=s.pc + size}]
        RETURN          -> case s.stack of off:size:_ -> [Left (Returned (s.memory, s.block.balances))]; _ -> error "fuck"
        GAS             -> [push (Literal 1) s]
        RETURNDATASIZE  -> [push (Literal 0) s]
        RETURNDATACOPY  -> case s.stack of doff:off:(Literal 0):r -> [Right s{stack=r, pc=s.pc+1}]; _  -> error "fuck"
        MISSING         -> [Left Reverted]
        INVALID         -> [Left Reverted]
        REVERT          -> [Left Reverted]
        CALL            -> semcall s
        x               -> error (show x ++ " NOT IMPLEMENTED" ++ show s.stack)
