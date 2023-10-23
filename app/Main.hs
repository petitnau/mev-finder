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

import Symbolic.Sem as SS
import Symbolic.Interface as SI
import Symbolic.SMT
import Symbolic.MEV

import Concrete.Sem as CS
import Concrete.Interface as CI
import Concrete.Uint256

import Crypto.Hash.Keccak (keccak256)
import qualified Data.ByteArray as BA
import qualified Env
import qualified Concrete.Memory as Memory
import Solidity.BytecodeDecode (decode, decodeProgram)

import qualified Control.Monad.State.Lazy as SM
import qualified Data.String as S
import qualified Data.List as L

import Data.Function ((&))

import Debug.Trace
import Utils
import Data.Maybe
import qualified Data.BitVector as BV
import Data.Tuple.HT (mapSnd, mapFst, thd3)
import Control.Monad (forM, forM_, filterM)
import Protolude (ifM, Bifunctor (bimap))
import Data.Functor ((<&>))
import Data.Tuple.Extra (both, second, first)
import Benchmark (timeItNamed)
import Test.Tasty.Runners (getTime)

import System.Log.Logger
import System.Log.Handler.Syslog
import System.Log.Handler.Simple
import System.Log.Handler (setFormatter)
import System.Log.Formatter
import System.Environment

type Checker =
    Integer
    -> [Integer]
    -> [Call]
    -> [(Integer, Integer)]
    -> [((Integer, Integer), Integer)]
    -> [((Integer, Integer, Integer), Integer)]
    -> [(StorageKey, Integer)]
    -> Program
    -> IO Integer

openProgram :: String -> IO Program
openProgram fn = do
    decodeProgram <$> readFile ("examples/bin/" ++ fn ++ ".bin-runtime")


eth, eur, dkk :: Integer
eth = 0
eur = 1
dkk = 2

contract, mallory, alice, bob, charlie, david :: Integer
contract = 0
mallory = 1
alice = 2
bob = 3
charlie = 4
david = 5

bw :: Checker -> IO Integer
bw check = do
    let attackers = [mallory]
    let worth = [(eth, 1)]
    let balances =
            [ ((eth, contract), 3000) 
            , ((eth, mallory),  200) ]
    let allowances = []
    let storage = [] 
    openProgram "BoundedWithdrawer" >>= check (-1) attackers [] worth balances allowances storage

cp :: Checker -> IO Integer
cp check = do
    let attackers = [mallory]
    let worth = [(eth, 1)]
    let balances =
            [ ((eth, contract), 10) 
            , ((eth, mallory),  200) 
            , ((eth, alice),    200) 
            , ((eth, bob),      200) 
            , ((eth, charlie),  200) 
            , ((eth, david),    200) ]
    let allowances = []
    let storage = [] 
    let mempool = 
            [ Call (fromInteger alice) "play" [] 90 
            , Call (fromInteger bob) "play" [] 90  
            , Call (fromInteger charlie) "play" [] 80
            , Call (fromInteger david) "play" [] 70 ]
    openProgram "CoinPusher" >>= check (-1) attackers mempool worth balances allowances storage

hl :: Checker -> IO Integer
hl check = do
    let attackers = [mallory]
    let worth = [(eth, 1)]
    let balances =
            [ ((eth, contract), 300)
            , ((eth, mallory),  25)
            , ((eth, alice),    25) ]
    let allowances = []
    let storage =  
            [ (SPos 0, 0xbd93a12ad13e81e9f6d5854d1e6163aae29ab3a1e1aae46441415b6338156780) ] 
    let mempool = 
            [ Call (fromInteger alice) "withdraw" [(Tbytes32, 0x90722a86d157231f39e98219c0c6084bbf7ca4b33e924dd3bdec1864ee8d4af8)] 0]
    openProgram "HashLock" >>= check (-1) attackers mempool worth balances allowances storage


amm :: Checker -> IO Integer
amm check = do
    let attackers = [mallory]
    let worth = [(eur, 2), (dkk, 1)]
    let balances =
            [ ((eth, contract), 1500)
            , ((eur, contract), 1500)
            , ((dkk, contract), 1500)
            , ((eth, mallory),  500)
            , ((eur, mallory),  500)
            , ((dkk, mallory),  500)
            , ((eth, alice),    500)
            , ((eur, alice),    500)
            , ((dkk, alice),    500) ]
    let allowances = 
            [ ((eur, contract, mallory), 500) 
            , ((dkk, contract, mallory), 500) 
            , ((eur, contract, alice),   500) 
            , ((dkk, contract, alice),   500)]
    let storage = 
            [ (SPos 0, 500) 
            , (SPos 1, 500) ]
    let mempool = 
            [Call (fromInteger alice) "swap0" [(TUint256, 500)] 0]
    openProgram "AMM" >>= check (-1) attackers mempool worth balances allowances storage

main :: IO ()
main = do
    [c, r] <- getArgs
    let checker = case r of "R" -> checkMEV'; "S" -> checkMEV
    let contract = case c of "bw" -> bw; "cp" -> cp; "hl" -> hl; "amm" -> amm

    --------------------

    s <- openlog "SyslogStuff" [PID] USER INFO
    updateGlobalLogger rootLoggerName (addHandler s)
    updateGlobalLogger "" (setLevel INFO)
    h <- fileHandler "debug.log" INFO >>= \lh -> return $
            setFormatter lh (simpleLogFormatter "[$time : $loggername : $prio] $msg")
    updateGlobalLogger "" (addHandler h)
    t <- getTime
    infoM "" $ "?? Running " ++ c ++ "-" ++ r
    infoM "" $ "?? Start at time " ++ show t

    --------------------
    --------------------

    res <- contract checker

    --------------------
    --------------------

    print res
