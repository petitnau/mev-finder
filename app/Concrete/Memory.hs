{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}

module Concrete.Memory where

import qualified Data.ByteArray as BA
import Data.DoubleWord
import Data.Word
import Concrete.Uint256
import Data.Parameterized (sndPair, viewSome)
import Data.Maybe (fromJust)
import Data.BitVector.Sized as BV
import Crypto.Hash.Keccak (keccak256)

takeExt :: BA.ByteArray a => Int -> a -> a
takeExt n = padr n . BA.take n

padr :: BA.ByteArray a => Int -> a -> a
padr n ba = ba `BA.append` BA.zero (n - BA.length ba)

applyPairs :: (a -> a -> b) -> [a] ->  [b]
applyPairs f = map (uncurry f) . uncurry zip . foldr (\a ~(x,y) -> (a:y,x)) ([],[])

getUint256 :: BA.ByteArray a => a -> Int -> Uint256
getUint256 ba offset =
    toUint256 . takeExt 32 . BA.drop offset $ ba

hashMem :: BA.ByteArray a => a -> Uint256 -> Uint256 -> (Uint256, Uint256)
hashMem ba offset size =
    let k = toUint256 . takeExt (touInt size) . BA.drop (touInt offset) $ ba in
    let v = toUint256 . keccak256 . fromUint256 $ k in
        (k, v)

toUint256 :: BA.ByteArray a => a -> Uint256
toUint256 ba =
    let z0 =  word8 <$>  BA.unpack ba in
    let z1 = applyPairs (BV.concat knownNat knownNat) z0 in
    let z2 = applyPairs (BV.concat knownNat knownNat) z1 in
    let z3 = applyPairs (BV.concat knownNat knownNat) z2 in
    let z4 = applyPairs (BV.concat knownNat knownNat) z3 in
    let z5 = applyPairs (BV.concat knownNat knownNat) z4 in
    Uint256 $ head z5

fromUint256 :: BA.ByteArray a => Uint256 -> a
fromUint256 = BA.pack . fromJust . BV.asBytesBE knownNat . toBV

updateBytes :: BA.ByteArray a => a -> a -> Int -> a
updateBytes ba1 ba2 off =
    let (ba1l, ba1r) = (takeExt off ba1, BA.drop (off + BA.length ba2) ba1)  in
    ba1l `BA.append` ba2 `BA.append` ba1r

byteAt :: Uint256 -> Uint256 -> Uint256
byteAt = apply2 (\bvx bvi -> BV.concat knownNat knownNat (BV.zero (knownNat @248)) . BV.word8 . (!! fromIntegral (asUnsigned bvi)) . fromJust . BV.asBytesLE knownNat $ bvx)
