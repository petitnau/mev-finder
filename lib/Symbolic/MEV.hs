{-# LANGUAGE OverloadedRecordDot #-}

module Symbolic.MEV where

import GenericSem
import Symbolic.Sem
import Symbolic.SMT
import Data.Composition

totalBalance :: Expr -> [Expr] -> Expr -> Expr
totalBalance tok actors bal = foldr1 bvadd . map ((bal @!) . (tok +++)) $ actors

totalWorth :: [Expr] -> [(Expr, Expr)] -> Expr -> Expr
totalWorth actors worth bal =
    foldr1 bvadd . map (\(t,v) -> v `bvmul` totalBalance t actors bal) $ worth

extractedValue :: [Expr] -> [(Expr, Expr)] -> State -> Expr
extractedValue attackers worth s = totalWorth attackers worth s.balance.balances `bvsub` totalWorth attackers worth (Var "Balances")

hasMEV :: [Expr] -> [(Expr, Expr)] -> State -> Expr
hasMEV attackers worth s = totalWorth attackers worth s.balance.balances `bvugt`  totalWorth attackers worth (Var "Balances")
