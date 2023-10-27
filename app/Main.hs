{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Prelude hiding (EQ, LT, GT, id)
import Ast
import SymbolicSem

import Crypto.Hash.Keccak (keccak256)
import qualified Data.ByteArray as BA
import qualified Env
import qualified Memory
import BytecodeDecode (decode)

import qualified Control.Monad.State.Lazy as SM
import Uint256
import qualified Data.String as S
import qualified Data.List as L

import Data.Function ((&))
import Debug.Trace

type StateBuilder a = SM.State State a


data Type =
    TUint256
    deriving Show

typeId :: Type -> String
typeId TUint256 = "uint256"

data Call =
    Call Uint256 String [(Type, Uint256)] Uint256
    deriving Show

callId :: Call -> BA.Bytes
callId (Call _ fn args _) =
    (encode . keccak256 . S.fromString $ fn ++ "(" ++ (L.intercalate "," . L.map (typeId . fst) $ args) ++ ")")
    `BA.append` BA.concat (L.map (Memory.fromWord . snd) args :: [BA.Bytes])
    where encode = BA.pack . BA.unpack . BA.take 4

baseState :: Program -> State
baseState ast = State
    { program = ast
    , pc = 0
    , block = BlockInfo
        { balances = EmptyStore }
    , stack = []
    , memory = EmptyStore
    , storage = EmptyStore
    , constraints = [] }

addBalance :: (Integer, Integer) -> State -> State
addBalance (k,v) s =
    s{block=s.block{balances=Store s.block.balances (Literal k) (Literal v)}}

main :: IO ()
main =
    baseState (Program $ decode simpleTransferBC) 
    & addBalance (0, 500)
    & sem
    & print

{-
baseState :: Program -> Call -> State
baseState ast c@(Call snd fn par val) = State
    { callState = CallState
        { id = 0
        , caller = snd
        , callValue = val
        , callData = callId c }
    , program = ast
    , pc = 0
    , origin = snd
    , block = BlockInfo
        { number = 0
        , timestamp = 0
        , balances = Env.empty }
    , stack = []
    , memory = BA.empty
    , storage = Env.empty}

addBalance :: (Uint256, Uint256) -> State -> State
addBalance kv s =
    s{block=s.block{balances=s.block.balances `Env.bind` kv}}

getOutput :: [Type] -> Result -> [Uint256]
getOutput [] (Returned _) = [] 
getOutput (TUint256:ts) (Returned m) = Memory.getWord m 0:getOutput ts (Returned $ BA.drop 32 m)

main :: IO ()
main =
--    baseState (Program $ decode getBC) (Call 1 "get" [(TUint256, 323)] 0)
    baseState (Program $ decode simpleTransferBC) (Call 1 "withdraw" [(TUint256, 323)] 0)
    & addBalance (0, 500)
    & sem
    & getOutput [TUint256]
    & print
-}

getBC :: String
getBC = "608060405234801561001057600080fd5b506004361061002b5760003560e01c80639507d39a14610030575b600080fd5b61004a600480360381019061004591906100b1565b610060565b60405161005791906100ed565b60405180910390f35b600060058261006f9190610137565b9050919050565b600080fd5b6000819050919050565b61008e8161007b565b811461009957600080fd5b50565b6000813590506100ab81610085565b92915050565b6000602082840312156100c7576100c6610076565b5b60006100d58482850161009c565b91505092915050565b6100e78161007b565b82525050565b600060208201905061010260008301846100de565b92915050565b7f4e487b7100000000000000000000000000000000000000000000000000000000600052601160045260246000fd5b60006101428261007b565b915061014d8361007b565b925082820190508082111561016557610164610108565b5b9291505056fea26469706673582212202a6aad6879abb7c462fc897f8b94cb44b0c0c9638f7500954520239fc3d276e764736f6c63430008120033"

simpleTransferBC :: String
simpleTransferBC = "608060405234801561001057600080fd5b506004361061002b5760003560e01c80632e1a7d4d14610030575b600080fd5b61004a6004803603810190610045919061010e565b61004c565b005b4781111561005957600080fd5b60003373ffffffffffffffffffffffffffffffffffffffff168260405161007f9061016c565b60006040518083038185875af1925050503d80600081146100bc576040519150601f19603f3d011682016040523d82523d6000602084013e6100c1565b606091505b50509050806100cf57600080fd5b5050565b600080fd5b6000819050919050565b6100eb816100d8565b81146100f657600080fd5b50565b600081359050610108816100e2565b92915050565b600060208284031215610124576101236100d3565b5b6000610132848285016100f9565b91505092915050565b600081905092915050565b50565b600061015660008361013b565b915061016182610146565b600082019050919050565b600061017782610149565b915081905091905056fea26469706673582212205ab90b00c32b8b835b89e81b4f98a6cc581c6e3e00a988d7ed07797f65b17b2464736f6c63430008120033"
