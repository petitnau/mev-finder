{-# LANGUAGE OverloadedRecordDot #-}

module Symbolic.MEV where

import GenericSem
import Symbolic.Sem
import Symbolic.SMT
import Data.Composition

totalBalance :: [Expr] -> Expr -> Expr
totalBalance actors bal = foldr1 bvadd . map (bal @!) $ actors

gain :: [Expr] -> Expr -> Expr -> Expr
gain actors initial final = totalBalance actors initial `bvsub` totalBalance actors final

extractedValue :: [Expr] -> State -> Expr
extractedValue attackers s = gain attackers s.balances (Var "Balances")

hasMEV :: [Expr] -> State -> Expr
hasMEV = (`bvugt` word 0) .* extractedValue
