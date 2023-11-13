{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE BlockArguments #-}

module Concrete.IERC20 where

import Concrete.Uint256
import Env
import Control.Monad (when)

data ERC20 = ERC20
    { balance    :: Env Uint256 Uint256
    , allowances :: Env (Uint256, Uint256) Uint256 }
    deriving Show

transfer :: Uint256 -> Uint256 -> Uint256 -> ERC20 -> Maybe (ERC20, Uint256)
transfer from to amt b0 = do
    when (Env.getDefault 0 b0.balance from < amt) Nothing
    let b1 = b0{balance=b0.balance `Env.bind` (from, Env.getDefault 0 b0.balance from `bvsub` amt)}
    let b2 = b1{balance=b1.balance `Env.bind` (to,   Env.getDefault 0 b1.balance to   `bvadd` amt)}
    return (b2, 1)

transferFrom :: Uint256 -> Uint256 -> Uint256 -> Uint256 -> ERC20 -> Maybe (ERC20, Uint256)
transferFrom auth from to amt b0 = do
    when (Env.getDefault 0 b0.allowances (auth, from) < amt) Nothing
    (b1, _) <- transfer from to amt b0
    let b2 = b1{allowances=b1.allowances `Env.bind` ((auth, from), Env.getDefault 0 b1.allowances (auth, from) `bvsub` amt)}
    return (b2, 1)

balanceOf :: Uint256 -> ERC20 -> Maybe (ERC20, Uint256)
balanceOf acc b0 = return (b0, Env.getDefault 0 b0.balance acc)
