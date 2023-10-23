{-# LANGUAGE ScopedTypeVariables #-}
module Memory where

import qualified Data.ByteArray as BA
import Data.DoubleWord
import Data.Word
import Uint256

takeExt :: BA.ByteArray a => Int -> a -> a
takeExt n = padr n . BA.take n

padr :: BA.ByteArray a => Int -> a -> a
padr n ba = ba `BA.append` BA.zero (n - BA.length ba)

applyPairs :: (a -> a -> b) -> [a] ->  [b]
applyPairs f = map (uncurry f) . uncurry zip . foldr (\a ~(x,y) -> (a:y,x)) ([],[]) 

getWord :: BA.ByteArray a => a -> Int -> Uint256
getWord ba offset = 
    toWord . takeExt 32 . BA.drop offset $ ba

toWord :: BA.ByteArray a => a -> Uint256
toWord ba = 
    let y = BA.unpack ba in
    let (z0 :: [Word16]) = applyPairs fromHiAndLo y in 
    let (z1 :: [Word32]) = applyPairs fromHiAndLo z0 in 
    let (z2 :: [Word64]) = applyPairs fromHiAndLo z1 in 
    let (z3 :: [Word128]) = applyPairs fromHiAndLo z2 in 
    let (z4 :: [Word256]) = applyPairs fromHiAndLo z3 in 
    Uint256 $ head z4

fromWord :: BA.ByteArray a => Uint256 -> a
fromWord (Uint256 w) = 
    let y = [w] in 
    let z0 = concatMap (\w -> [hiWord w, loWord w]) y in
    let z1 = concatMap (\w -> [hiWord w, loWord w]) z0 in
    let z2 = concatMap (\w -> [hiWord w, loWord w]) z1 in
    let z3 = concatMap (\w -> [hiWord w, loWord w]) z2 in
    let z4 = concatMap (\w -> [hiWord w, loWord w]) z3 in
    BA.pack z4

updateBytes :: BA.ByteArray a => a -> a -> Int -> a
updateBytes ba1 ba2 off =
    let (ba1l, ba1r) = (takeExt off ba1, BA.drop (off + BA.length ba2) ba1)  in
    ba1l `BA.append` ba2 `BA.append` ba1r
