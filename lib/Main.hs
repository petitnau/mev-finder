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
import Concrete.IERC20
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
import Data.BitVector.Sized (zero, knownNat, one)
import qualified Data.BitVector.Sized as BV
import Data.Tuple.HT (mapSnd, mapFst, thd3)
import Control.Monad (forM, forM_, filterM)
import Protolude (ifM)
import Data.Functor ((<&>))

liftR :: (SS.State -> a) -> ((Expr, SS.State, Integer) -> [a])
liftR f = \(_,s,_) -> [f s]

baseAxioms :: [Expr]
baseAxioms =
    [ Var "Id" `eq` word 0 ]

memPoolAxioms :: Program -> [Expr]
memPoolAxioms p = 
            let result =
                    CI.baseState p
                    (Call 1 "withdraw" [(Tbytes32, 0x90722a86d157231f39e98219c0c6084bbf7ca4b33e924dd3bdec1864ee8d4af8)] 0)
                    & addStorage (0, 0xbd93a12ad13e81e9f6d5854d1e6163aae29ab3a1e1aae46441415b6338156780)
                    & setBalance (0, 500)
                    & setBalance (1, 1)
                    & CS.sem
            in getConstraints result

smain :: IO ()
smain = do
    let attackers = [word 1]
    let customAxioms = buildAxioms attackers
            [ (word 0, word 500)
            , (word 1, word 1) ]
            [ ]
    let contract = "HashLock"
    program <- decodeProgram <$> readFile ("examples/bin/" ++ contract ++ ".bin-runtime")

    let mAxioms = memPoolAxioms program
    check 1 (liftR $ hasMEV attackers) (liftR $ extractedValue attackers) mAxioms baseAxioms customAxioms program >>= print

cmain :: IO ()
cmain = do
    let eur = 0x230A1AC45690B9Ae1176389434610B9526d2f21b 
    let dkk = 0x330a1Ac45690b9ae1176389434610b9526D2f21B
    let commit = 0x90722a86d157231f39e98219c0c6084bbf7ca4b33e924dd3bdec1864ee8d4af8
    let contract = "HashLock" --AMM
    program <- decodeProgram <$> readFile ("examples/bin/" ++ contract ++ ".bin-runtime")
    let result = CI.baseState program
            --(Call 1 "addliq" [(TUint256, 350), (TUint256, 350)] 0)
            (Call 1 "withdraw" [(Tbytes32, commit)] 0)
            & addStorage (0, 0xbd93a12ad13e81e9f6d5854d1e6163aae29ab3a1e1aae46441415b6338156780)
            & setBalance (0, 500)
            -- & setBalance (1, 10)
            -- & setERC20Balance (eur, 1, 500)
            -- & setERC20Allowance (eur, 0, 1, 500)
            -- & setERC20Balance (dkk, 1, 500)
            -- & setERC20Allowance (dkk, 0, 1, 500)
            & CS.sem
    putStrLn "\n"
    putStrLn $ prettyResult result
    putStrLn "--   VALUE  --"
    print $ getOutput [] result

main :: IO ()
main = smain
