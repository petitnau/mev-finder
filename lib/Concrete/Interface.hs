{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE RecordWildCards #-}

module Concrete.Interface where

import Ast
import Utils
import GenericSem
import Concrete.Sem
import Concrete.Memory
import Concrete.Uint256
import Concrete.Memory as Memory
import qualified Data.String as S
import qualified Data.List as L
import qualified Data.ByteArray as BA
import qualified Env 
import Symbolic.SMT (Expr(..))

import Prelude hiding (id)
import Debug.Trace (traceShowId, traceWith)

data Type
  = TUint256 
  | Tbytes32
    deriving Show

typeId :: Type -> String
typeId TUint256 = "uint256"
typeId Tbytes32 = "bytes32"

data Call =
    Call Uint256 String [(Type, Uint256)] Uint256
    deriving Show

callId :: Call -> BA.Bytes
callId (Call _ fn args _) =
    funId (fn ++ "(" ++ (L.intercalate "," . L.map (typeId . fst) $ args) ++ ")")
    `BA.append` BA.concat (L.map (Memory.fromUint256 . snd) args :: [BA.Bytes])


emptyState :: Program -> State
emptyState prog = State
    { id = 0
    , caller = 0
    , callValue = 0
    , number = 0
    , timestamp = 0
    , callData = BA.empty
    , program = prog
    , pc = 0
    , balance = Balance { balances = Env.empty, allowances = Env.empty }
    , stack = []
    , memory = BA.empty
    , returned = BA.empty
    , storage = Env.empty
    , extra = Extra { constraints = [] } }

baseState :: Program -> Call -> State
baseState prog c@(Call snd fn par val) = (emptyState prog)
    { caller = snd
    , callValue = val
    , callData = callId c }

setBalance :: ((Uint256, Uint256), Uint256) -> State -> State
setBalance kv s =
    s{balance=s.balance{balances=s.balance.balances `Env.bind` kv}}

setAllowance :: ((Uint256, Uint256, Uint256), Uint256) -> State -> State
setAllowance kv s =
    s{balance=s.balance{allowances=s.balance.allowances `Env.bind` kv}}

setStorage :: (Uint256, Uint256) -> State -> State
setStorage kv s =
    s{storage=s.storage `Env.bind` kv}

setCore :: CoreState -> State -> State
setCore CoreState{..} s = 
    s{balance=s.balance{balances=coreBalance, allowances=coreAllowances}, storage=coreStorage}

getCore :: State -> CoreState
getCore s = CoreState{coreStorage=s.storage, coreBalance=s.balance.balances, coreAllowances=s.balance.allowances}

getOutput :: [Type] -> Result -> [Uint256]
getOutput [] (Returned _) = []
getOutput (TUint256:ts) (Returned (m, s)) = Memory.getUint256 m 0:getOutput ts (Returned (BA.drop 32 m, s))

getConstraints :: Result -> [Expr]
getConstraints (Returned (_, s)) = s.extra.constraints
getConstraints Reverted = []

sequenceCalls :: State -> [(Integer, Integer, Integer, Integer)] -> State
sequenceCalls s [] = s
sequenceCalls s ((caller, callvalue, calldata, calldatasize):cs) = 
    let s' = s
            { caller = fromInteger caller
            , callValue = fromInteger callvalue
            , callData = Memory.takeExt (fromInteger calldatasize) $ Memory.fromSizedInteger 512 calldata
            , pc = 0
            , stack = []
            , memory = BA.empty
            , returned = BA.empty } in
    let (Returned (r, s'')) = sem s' in
    sequenceCalls s'' cs
