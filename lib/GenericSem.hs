{-# LANGUAGE OverloadedRecordDot #-}

module GenericSem where

import Ast
import Data.Bifunctor

data StaticInfo

data GState x b v m s = State
    { id :: v
    , caller :: v
    , callValue :: v
    , number :: v
    , timestamp :: v
    , callData :: m

    , extra :: x
    , program :: Program
    , pc :: Int
    , balance :: b
    , stack :: [v]
    , memory :: m
    , returned :: m
    , storage :: s }
    deriving Show

data GResult x b v m s
    = Returned (m, GState x b v m s)
    | Reverted
    deriving Show

lookn  :: Int -> GState x b v m s -> Either (GResult x b v m s) (GState x b v m s, v)
lookn 0 s@State{stack = a:r} = Right (s, a)
lookn n s@State{stack = a:r} = lookn (n-1) s{stack=r} >>= \(_, v) -> Right (s, v)
lookn _ _ = Left Reverted

pop  :: GState x b v m s -> Either (GResult x b v m s) (GState x b v m s, v)
pop2 :: GState x b v m s -> Either (GResult x b v m s) (GState x b v m s, v, v)
pop3 :: GState x b v m s -> Either (GResult x b v m s) (GState x b v m s, v, v, v)
pop  s@State{stack = a:r}     = Right (s{stack=r}, a)
pop  _ = Left Reverted
pop2 s@State{stack = a:b:r}   = Right (s{stack=r}, a, b)
pop2 _ = Left Reverted
pop3 s@State{stack = a:b:c:r} = Right (s{stack=r}, a, b, c)
pop3 _ = Left Reverted

popn :: Int -> GState x b v m s -> Either (GResult x b v m s) (GState x b v m s, [v])
popn 0 s                    = Right (s, [])
popn n s@State{stack = a:r} = second (a :) <$> popn (n-1) s{stack=r}
popn _ _                    = Left Reverted

pop_ :: GState x b v m s -> Either (GResult x b v m s) (GState x b v m s)
pop_ s = fst <$> pop s

push :: v -> GState x b v m s -> Either (GResult x b v m s) (GState x b v m s)
push v s = Right s{stack=v:s.stack}

pushn :: [v] -> GState x b v m s -> Either (GResult x b v m s) (GState x b v m s)
pushn vs s = Right s{stack=vs++s.stack}

semuop :: (v -> v)           -> GState x b v m s -> Either (GResult x b v m s) (GState x b v m s)
sembop :: (v -> v -> v)      -> GState x b v m s -> Either (GResult x b v m s) (GState x b v m s)
semtop :: (v -> v -> v -> v) -> GState x b v m s -> Either (GResult x b v m s) (GState x b v m s)
semuop op s = pop  s >>= \(s, a)       -> push (op a) s
sembop op s = pop2 s >>= \(s, a, b)    -> push (op a b) s
semtop op s = pop3 s >>= \(s, a, b, c) -> push (op a b c) s

increasePC :: GState x b v m s -> GState x b v m s
increasePC s = s{pc=s.pc+1}
