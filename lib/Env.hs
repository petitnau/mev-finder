module Env where
import Data.Maybe
import Data.Composition
import Data.Function
import Data.List

import qualified Data.Map.Strict as M

type Env a b = M.Map a b

empty :: Env a b 
empty = M.empty

domain :: Eq a => Env a b -> [a]
domain = map fst . M.toList

bind :: (Ord a, Ord b) => Env a b -> (a, b) -> Env a b
bind d (k,v) = M.insert k v d

get :: (Ord a, Ord b) => Env a b -> a -> Maybe b 
get = flip M.lookup

getDefault :: (Ord a, Ord b) => b -> Env a b -> a -> b 
getDefault x = fromMaybe x .* get 

toList :: Env a b -> [(a,b)]
toList = M.toList
