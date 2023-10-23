{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}

module Concrete.Memory where

import qualified Data.ByteArray as BA
import qualified Data.ByteString.Lazy.Internal as BS
import qualified Data.ByteString as BSS
import Data.DoubleWord
import Data.Word
import Concrete.Uint256
import Data.Parameterized (sndPair, viewSome)
import Data.Maybe (fromJust)
import qualified Data.BitVector as BV
import Data.Binary as B
import Crypto.Hash.Keccak (keccak256)
import Debug.Trace (traceShowId)
import Data.Bits
import Data.List

takeExt :: BA.ByteArray a => Int -> a -> a
takeExt n = padr n . BA.take n

padr :: BA.ByteArray a => Int -> a -> a
padr n ba = ba `BA.append` BA.zero (n - BA.length ba)

padl :: BA.ByteArray a => Int -> a -> a
padl n ba = BA.zero (n - BA.length ba) `BA.append` ba

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
    let z0 = BV.bitVec 8 <$> BA.unpack ba in
    let z1 = applyPairs (BV.#) z0 in
    let z2 = applyPairs (BV.#) z1 in
    let z3 = applyPairs (BV.#) z2 in
    let z4 = applyPairs (BV.#) z3 in
    let z5 = applyPairs (BV.#) z4 in
    Uint256 $ head z5

fromUint256 :: BA.ByteArray a => Uint256 -> a
fromUint256 = BA.pack . unpack

updateBytes :: BA.ByteArray a => a -> a -> Int -> a
updateBytes ba1 ba2 off =
    let (ba1l, ba1r) = (takeExt off ba1, BA.drop (off + BA.length ba2) ba1)  in
    ba1l `BA.append` ba2 `BA.append` ba1r

byteAt :: Uint256 -> Uint256 -> Uint256
byteAt b i = Uint256 $ BV.extract (255 - 8*(touInteger i)) ((256 - 8*(touInteger i + 1))) (toBV b)

fromSizedInteger :: BA.ByteArray a => Int -> Integer -> a
fromSizedInteger s = padl s .  BA.pack . reverse . unroll 

toInteger :: BA.ByteArray a => a -> Integer
toInteger = foldl f 0 . BA.unpack
  where
    f a b = a `shiftL` 8 .|. fromIntegral b

unroll :: Integer -> [Word8]
unroll = unfoldr step
  where
    step 0 = Nothing
    step i = Just (fromIntegral i, i `shiftR` 8)

roll :: [Word8] -> Integer
roll   = foldr unstep 0
  where
    unstep b a = a `shiftL` 8 .|. fromIntegral b
