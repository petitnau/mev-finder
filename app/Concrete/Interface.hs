{-# LANGUAGE OverloadedRecordDot #-}

module Concrete.Interface where

import Ast
import GenericSem
import Concrete.Sem
import Concrete.Utils
import Concrete.IERC20
import Concrete.Memory
import Concrete.Uint256
import Concrete.Memory as Memory
import qualified Data.String as S
import qualified Data.List as L
import qualified Data.ByteArray as BA
import qualified Env 

import Prelude hiding (id)

data Type
  = TUint256 
  | Tbytes32
  | Tbytes
    deriving Show

typeId :: Type -> String
typeId TUint256 = "uint256"
typeId Tbytes32 = "bytes32"
typeId Tbytes   = "bytes"

data Call =
    Call Uint256 String [(Type, Uint256)] Uint256
    deriving Show

callId :: Call -> BA.Bytes
callId (Call _ fn args _) =
    funId (fn ++ "(" ++ (L.intercalate "," . L.map (typeId . fst) $ args) ++ ")")
    `BA.append` BA.concat (L.map (Memory.fromUint256 . snd) args :: [BA.Bytes])


baseState :: Program -> Call -> State
baseState prog c@(Call snd fn par val) = State
    { id = 0
    , caller = snd
    , callValue = val
    , number = 0
    , timestamp = 0
    , callData = callId c
    , program = prog
    , pc = 0
    , balances = Balance 
        { eth=Env.empty
        , tok=Env.empty }
    , stack = []
    , memory = BA.empty
    , returned = BA.empty
    , storage = Env.empty
    , extra = () }

setBalance :: (Uint256, Uint256) -> State -> State
setBalance kv s =
    s{balances=s.balances{eth=s.balances.eth `Env.bind` kv}}

setERC20Balance :: (Uint256, Uint256, Uint256) -> State -> State
setERC20Balance (tid, addr, amt) s =
    let t0 = Env.getDefault (ERC20 { balance=Env.empty, allowances=Env.empty } ) s.balances.tok tid in
    let t1 = t0{balance=t0.balance `Env.bind` (addr, amt)} in
    s{balances=s.balances{tok=s.balances.tok `Env.bind` (tid, t1)}}

setERC20Allowance :: (Uint256, Uint256, Uint256, Uint256) -> State -> State
setERC20Allowance (tid, from, auth, amt) s =
    let t0 = Env.getDefault (ERC20 { balance=Env.empty, allowances=Env.empty } ) s.balances.tok tid in
    let t1 = t0{allowances=t0.allowances `Env.bind` ((from, auth), amt)} in
    s{balances=s.balances{tok=s.balances.tok `Env.bind` (tid, t1)}}

addStorage :: (Uint256, Uint256) -> State -> State
addStorage kv s =
    s{storage=s.storage `Env.bind` kv}

getOutput :: [Type] -> Result -> [Uint256]
getOutput [] (Returned _) = []
getOutput (TUint256:ts) (Returned (m, s)) = Memory.getUint256 m 0:getOutput ts (Returned (BA.drop 32 m, s))

