module Env where

type Env a b = [(a,b)]

empty :: Env a b 
empty = []

bind :: Env a b -> (a, b) -> Env a b
bind d e = e:d

get :: Eq a => Env a b -> a -> b 
get [] a = error "TODO"
get ((a0, b0):xs) a = if a0 == a then b0 else get xs a 
