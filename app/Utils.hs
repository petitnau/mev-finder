module Utils where

import Data.Bits

modExp :: Integer -> Integer -> Integer -> Integer
modExp b 0 m = 1
modExp b e m = t * modExp ((b * b) `mod` m) (shiftR e 1) m `mod` m
    where t = if testBit e 0 then b `mod` m else 1


printMem :: String -> String
printMem = (++) "\n" . insert 50 '\n' . insert 24 '|' . insert 2 ' '
    where
        insert :: Int -> Char -> String -> String
        insert 0 y xs = xs
        insert n y [] = []
        insert n y xs
            | length xs < n = xs
            | otherwise = take n xs ++ [y] ++ insert n y (drop n xs)