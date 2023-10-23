module Utils where

import Data.Bits

modExp :: Integer -> Integer -> Integer -> Integer
modExp b 0 m = 1
modExp b e m = t * modExp ((b * b) `mod` m) (shiftR e 1) m `mod` m
    where t = if testBit e 0 then b `mod` m else 1
