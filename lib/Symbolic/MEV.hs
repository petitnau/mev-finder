{-# LANGUAGE OverloadedRecordDot #-}

module Symbolic.MEV where

import GenericSem
import Symbolic.Sem
import Symbolic.SMT
import Data.Composition

totalBalance :: [Expr] -> Expr -> Expr
totalBalance actors bal = foldr1 bvadd . map ((bal @!)) $ actors

totalWorth :: [Expr] -> Expr -> Expr
totalWorth actors bal =
    totalBalance actors bal

extractedValue :: [Expr] -> State -> Expr
extractedValue attackers s = totalWorth attackers s.balances `bvsub` totalWorth attackers (Var "Balances")

hasMEV :: [Expr] -> State -> Expr
hasMEV attackers s = totalWorth attackers s.balances `bvugt` totalWorth attackers (Var "Balances")

