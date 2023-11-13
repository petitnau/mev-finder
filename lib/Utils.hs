module Utils where

import Data.Bits
import Control.Monad (join)
import Data.Functor ((<&>))
import Data.String.HT (trim)

modExp :: Integer -> Integer -> Integer -> Integer
modExp b 0 m = 1
modExp b e m = t * modExp ((b * b) `mod` m) (shiftR e 1) m `mod` m
    where t = if testBit e 0 then b `mod` m else 1

printMem :: String -> String
printMem = (++) "\n" . trim . insert 104 "\n" . insert 24 "| " . insert 2 " "
    where
        insert :: Int -> String -> String -> String
        insert 0 y xs = xs
        insert n y [] = []
        insert n y xs
            | length xs < n = xs
            | otherwise = take n xs ++ y ++ insert n y (drop n xs)

padBinary :: Int -> String -> String
padBinary l s = replicate (l - length s) '0' ++ s

(>>>=) :: (Monad m1, Monad m2, Traversable m2) => m1 (m2 a) -> (a -> m1 (m2 b)) -> m1 (m2 b)
(>>>=) m f = (m >>= mapM f) <&> join

mapPairSame :: (a -> b) -> (a,a) -> (b,b)
mapPairSame f (x, y) = (f x, f y)

orM :: Monad m => m Bool -> m Bool -> m Bool
orM m1 m2 = m1 >>= \x -> if x then return True else m2

anyM :: (Foldable t, Monad m) => (a -> m Bool) -> t a -> m Bool
anyM f = foldr (orM . f) (pure False)
