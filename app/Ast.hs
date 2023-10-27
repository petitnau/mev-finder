{-# LANGUAGE OverloadedRecordDot #-}

module Ast where

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

import Debug.Trace
import qualified Data.List as L

data Ast
    = STOP
    | ADD
    | MUL
    | SUB
    | DIV
    | SDIV
    | MOD
    | SMOD
    | ADDMOD
    | MULMOD
    | EXP
    | SIGNEXTEND
    | LT
    | GT
    | SLT
    | SGT
    | EQ
    | ISZERO
    | AND
    | OR
    | XOR
    | NOT
    | BYTE
    | SHL
    | SHR
    | SAR
    | SHA3
    | ADDRESS
    | BALANCE
    | ORIGIN
    | CALLER
    | CALLVALUE
    | CALLDATALOAD
    | CALLDATASIZE
    | CALLDATACOPY
    | CODESIZE
    | CODECOPY
    | GASPRICE
    | EXTCODESIZE
    | EXTCODECOPY
    | RETURNDATASIZE
    | RETURNDATACOPY
    | EXTCODEHASH
    | BLOCKHASH
    | COINBASE
    | TIMESTAMP
    | NUMBER
    | PREVRANDAO
    | DIFFICULTY
    | GASLIMIT
    | CHAINID
    | SELFBALANCE
    | BASEFEE
    | POP
    | MLOAD
    | MSTORE
    | MSTORE8
    | SLOAD
    | SSTORE
    | JUMP
    | JUMPI
    | PC
    | MSIZE
    | GAS
    | JUMPDEST
    | PUSH Int Integer
    | DUP Int
    | SWAP Int
    | LOG Int
    | CREATE
    | CALL
    | CALLCODE
    | RETURN
    | DELEGATECALL
    | CREATE2
    | STATICCALL
    | REVERT
    | INVALID
    | SELFDESTRUCT
    | MISSING 
    deriving Show

newtype Program = Program [Ast]
instance Show Program where show = const "Program"

opAt :: Program -> Int -> Ast
opAt (Program []) n = error "Not found"
opAt (Program (x:_)) 0 = x 
opAt (Program ((PUSH s _):xs)) n = opAt (Program xs) (n-s-1) 
opAt (Program (_:xs)) n = opAt (Program xs) (n-1) 

