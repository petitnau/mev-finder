{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE LambdaCase #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Redundant lambda" #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE BlockArguments #-}

module Main where

import Prelude hiding (EQ, LT, GT, id)
import Ast
import GenericSem

-- import SymbolicSem
import ConcreteSem
import Uint256
import Z3

import Crypto.Hash.Keccak (keccak256)
import qualified Data.ByteArray as BA
import qualified Env
import qualified Memory
import BytecodeDecode (decode, decodeProgram)

import qualified Control.Monad.State.Lazy as SM
import qualified Data.String as S
import qualified Data.List as L

import Data.Function ((&))

import Debug.Trace
import Utils
import Data.Maybe
import Data.BitVector.Sized (zero, knownNat, one)
import qualified Data.BitVector.Sized as BV
import Data.Tuple.HT (mapSnd, mapFst, thd3)
import Control.Monad (forM, forM_, filterM)
import Protolude (ifM)
import Data.Functor ((<&>))

type StateBuilder a = SM.State State a

data Type =
    TUint256
    deriving Show

typeId :: Type -> String
typeId TUint256 = "uint256"

{-
baseState :: Program -> State
baseState prog = State
    { id = Var "Id"
    , caller = Var "Caller1"
    , callValue = Var "CallValue1"
    , callData = Var "CallData1"
    , number = Var "BlockNumber"
    , timestamp = Var "Timestamp"
    , program = prog
    , pc = 0
    , balances = Var "Balances"
    , stack = []
    , memory = mem 0
    , storage = Var "Storage"
    , extra = Extra {constraints = [], callDataSize = Var "CallDataSize1"} }

baseDecls :: [Decl]
baseDecls =
    [ DeclVar "Id" tword
    , DeclVar "BlockNumber" tword
    , DeclVar "Timestamp" tword
    , DeclVar "Balances" (TArray tword tword)
    , DeclVar "Storage" (TArray tword tword) 
    , DeclFun "Hash" tmem tword ]

baseAxioms :: [Expr]
baseAxioms =
    [ ForAll "x" tmem (ForAll "y" tmem (lnot (Var "x" `eq` Var "y") `implies` (lnot (UnOp Hash (Var "x") `eq` UnOp Hash (Var "y")))))
    , Var "Id" `eq` word 0 ]

declsCall :: Integer -> [Decl]
declsCall i =
    [ DeclVar ("Caller" ++ show i) tword
    , DeclVar ("CallValue" ++ show i) tword
    , DeclVar ("CallDataSize" ++ show i) tword
    , DeclVar ("CallData" ++ show i) tmem ]

axiomsCall :: Integer -> [Expr]
axiomsCall i =
    [ Var ("CallData" ++ show i) `eq` (Var ("CallData" ++ show i) `bvand` ((mem 0 `bvsub` mem 1) `bvshl` (mem 1024 `bvsub` ((LBitVec 768 0 +++ Var ("CallDataSize" ++ show i)) `bvmul` mem 8))))
    , ForAll "x" tword ((Var "Balances" @! Var "x") `bvult` word 0x100000000000000000000000000000000) ]

check_ :: Integer -> State -> [Decl] -> [Expr] -> ((Expr, State, Integer) -> [Expr]) -> (Integer -> [Expr]) -> Integer -> IO Bool
check_ 0 state decls axioms checks customAxioms i = return False
check_ depth state decls axioms checks customAxioms i = do
    putStrLn $ "Checking depth: " ++ show i
    let decls'  = decls  ++ declsCall i
    let axioms' = axioms ++ axiomsCall i
    let results = mapMaybe (\case Returned r -> Just r; _ -> Nothing) (sem state)
    let constraintss2 = map (\(r,s) -> (r, s, checks (r,s,i) ++ customAxioms i ++ axioms' ++ s.extra.constraints)) results
    anyM (smtcheck decls' . thd3) constraintss2 `orM`
        do
            let constraintss = map (\(r,s) -> (r, s, customAxioms i ++ axioms' ++ s.extra.constraints)) results
            feasible <- filterM (smtcheck decls' . thd3) constraintss
            results <- forM feasible (\(r, s, _) ->
                let state' = s{
                    caller = Var ("Caller" ++ show (i+1)),
                    callValue = Var ("CallValue" ++ show (i+1)),
                    callData = Var ("CallData" ++ show (i+1)),
                    pc = 0,
                    memory = mem 0,
                    extra=s.extra{callDataSize = Var ("CallDataSize" ++ show (i+1))} } in
                check_ (depth-1) state' decls' axioms' checks customAxioms (i+1))
            return $ or results

check :: Integer -> ((Expr, State, Integer) -> [Expr]) -> (Integer -> [Expr]) -> Program -> IO Bool
check depth checks customAxioms program =
    check_ depth (baseState program) baseDecls baseAxioms checks customAxioms 1

storeAxioms :: Expr -> [(Expr, Expr)] -> [Expr]
storeAxioms e = map (\(k,v) -> (e @! k) `eq` v)

oneOf :: Expr -> [Expr] -> Expr
oneOf e = foldr1 bvor . map (e `eq`)

buildAxioms :: [Expr] -> [(Expr, Expr)] -> [(Expr, Expr)] -> Integer -> [Expr]
buildAxioms attackers balances storage n =  
    [ Var ("Caller" ++ show i) `oneOf` attackers | i <- [1..n]] ++
    storeAxioms (Var "Balances") balances ++
    storeAxioms (Var "Storage") storage

hasMEV :: [Expr] -> (Expr, State, Integer) -> [Expr]
hasMEV attackers (_,s,_) = 
    let atkworth b = foldr1 bvadd . map (b @!) $ attackers in
    [ atkworth s.balances `bvugt` atkworth (Var "Balances") ]

main :: IO ()
main = do
    let attackers = [word 1]
    let axioms = buildAxioms attackers
            [ (word 0,   word 500)
            , (word 1,   word 1) ] 
            [ ]
            -- (UnOp Hash(LBitVec 512 0 +++ LBitVec 256 1 +++ LBitVec 256 0), word 10) 
    let contract = "Withdrawer"
    program <- decodeProgram <$> readFile ("examples/bin/" ++ contract ++ ".bin-runtime")
    check 1 (hasMEV attackers) axioms program >>= print
-}
{-
-}
data Call =
    Call Uint256 String [(Type, Uint256)] Uint256
    deriving Show

callId :: Call -> BA.Bytes
callId (Call _ fn args _) =
    (encode . keccak256 . S.fromString $ fn ++ "(" ++ (L.intercalate "," . L.map (typeId . fst) $ args) ++ ")")
    `BA.append` BA.concat (L.map (Memory.fromUint256 . snd) args :: [BA.Bytes])
    where encode = BA.pack . BA.unpack . BA.take 4

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
    , balances = Env.empty
    , stack = []
    , memory = BA.empty
    , storage = Env.empty
    , extra = () }

addBalance :: (Uint256, Uint256) -> State -> State
addBalance kv s =
    s{balances=s.balances `Env.bind` kv}

addStorage :: (Uint256, Uint256) -> State -> State
addStorage kv s =
    s{storage=s.storage `Env.bind` kv}

getOutput :: [Type] -> Result -> [Uint256]
getOutput [] (Returned _) = []
getOutput (TUint256:ts) (Returned (m, s)) = Memory.getUint256 m 0:getOutput ts (Returned (BA.drop 32 m, s))

main :: IO ()
main = do
    let contract = "AMM"
    program <- decodeProgram <$> readFile ("examples/bin/" ++ contract ++ ".bin-runtime")
    let result = baseState program
            -- (Call 1 "get" [] 0)
            (Call 1 "addliq" [(TUint256, 350), (TUint256, 350)] 10)
            & addBalance (0, 500)
            & addBalance (1, 10)
            & sem
    putStrLn "\n"
    putStrLn $ prettyResult result
    putStrLn "--   VALUE  --"
    print $ getOutput [] result
