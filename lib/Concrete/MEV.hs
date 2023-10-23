{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE TupleSections #-}

module Concrete.MEV where

import GenericSem
import Concrete.Uint256
import Concrete.Sem
import Data.Composition
import Env (Env(..), get, toList)
import Data.Maybe (fromJust)
import Debug.Trace (traceShowId)

totalBalance :: Uint256 -> [Uint256] -> Env (Uint256, Uint256) Uint256 -> Uint256
totalBalance tok actors bal = foldr1 bvadd . map (fromJust . Env.get bal . (tok,)) $ actors

totalWorth :: [Uint256] -> Env Uint256 Uint256 -> Env (Uint256, Uint256) Uint256 -> Uint256
totalWorth actors worth bal =
    foldr1 bvadd . map (\(t,v) -> v `bvmul` totalBalance t actors bal) $ toList worth

hasMEV :: [Uint256] -> Env Uint256 Uint256 -> (State, State) -> Bool
hasMEV attackers worth (s0,sf) = totalWorth attackers worth s0.balance.balances `bvugt`  totalWorth attackers worth s0.balance.balances

extractedValue :: [Uint256] -> Env Uint256 Uint256 -> (State, State) -> Uint256
extractedValue attackers worth (s0,sf) = 
    let pre = totalWorth attackers worth s0.balance.balances in
    let post = totalWorth attackers worth sf.balance.balances in
    if post `bvugt` pre then post `bvsub` pre else 0
