module Env where
import Data.Maybe
import Data.Composition
import Data.Function
import Data.List

newtype Env a b = Env [(a,b)]

instance (Show a, Show b, Ord a) => Show (Env a b) where
    show (Env ls) = show . sortBy (compare `on` fst) . nubBy ((==) `on` fst) $ ls

empty :: Env a b 
empty = Env []

domain :: Eq a => Env a b -> [a]
domain (Env ls) = nub . map fst $ ls

bind :: Env a b -> (a, b) -> Env a b
bind (Env d) e = Env (e:d)

get :: Eq a => Env a b -> a -> Maybe b 
get (Env []) a = Nothing
get (Env ((a0, b0):xs)) a = if a0 == a then Just b0 else get (Env xs) a 

getDefault :: Eq a => b -> Env a b -> a -> b 
getDefault x = fromMaybe x .* get 
