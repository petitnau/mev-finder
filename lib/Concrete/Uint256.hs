{-# LANGUAGE DataKinds #-}

module Concrete.Uint256 where

import Data.DoubleWord
import Data.Bits
import Utils
import Data.Word (Word8)
import Data.BitVector as BV
import Data.Maybe (fromJust)
import GHC.Natural (Natural)
import Data.Tuple.HT
import Debug.Trace

newtype Uint256 = Uint256 { toBV :: BitVector }
    deriving (Eq, Ord)

instance Show Uint256 where
    show (Uint256 a) = show a

apply1 :: (BitVector -> BitVector) -> Uint256 -> Uint256
apply1 f = Uint256 . f . toBV

apply1_ :: (BitVector -> r) -> Uint256 -> r
apply1_ f = f . toBV

apply2 :: (BitVector -> BitVector -> BitVector) -> Uint256 -> Uint256 -> Uint256
apply2 f = curry $ Uint256 . uncurry f . bimap' toBV

apply2f :: (BitVector -> b -> BitVector) -> Uint256 -> b -> Uint256
apply2f f = curry $ Uint256 . uncurry f . mapFst toBV

apply2_ :: (BitVector -> BitVector -> r) -> Uint256 -> Uint256 -> r
apply2_ f = curry $  uncurry f . bimap' toBV

bvadd, bvmul, bvsub, bvuquot, bvsquot, bvurem, bvsrem :: Uint256 -> Uint256 -> Uint256
bvadd   = apply2 (+)
bvmul   = apply2 (*)
bvsub   = apply2 (-)
bvuquot = apply2 (quot)
bvsquot = apply2 (sdiv)
bvurem  = apply2 (rem)
bvsrem  = apply2 (srem)

bvult, bvugt, bvslt, bvsgt :: Uint256 -> Uint256 -> Bool
bvult = apply2_ (<)
bvugt = flip bvult
bvslt = apply2_ (slt)
bvsgt = flip bvslt

bvand, bvor, bvxor, bvshl, bvlshr, bvashr :: Uint256 -> Uint256 -> Uint256
bvand  = apply2 (.&.)
bvor   = apply2 (.|.)
bvxor  = apply2 (xor)
bvshl  = apply2 (shl)
bvlshr = apply2 (shr)
bvashr = apply2 (ashr)


bvabs, bvsignum, bvcomplement :: Uint256 -> Uint256
bvabs        = apply1 (abs)
bvsignum     = apply1 (signum)
bvcomplement = apply1 (complement)

instance Num Uint256 where
    (+) = bvadd
    (*) = bvmul
    abs = bvabs
    signum = bvsignum
    fromInteger = Uint256 . bitVec 256  
    (-) = bvsub

toBool :: Uint256 -> Bool
toBool (Uint256 x) = x /= bitVec 256 0

fromBool :: Bool -> Uint256
fromBool False = Uint256 $ bitVec 256 0
fromBool True = Uint256 $ bitVec 256 1

touInteger :: Uint256 -> Integer
touInteger = uint . toBV

touInt:: Uint256 -> Int
touInt = fromIntegral . touInteger

toWord256 :: Uint256 -> Word256
toWord256 = fromIntegral . touInteger

lbyte :: Uint256 -> Word8
lbyte = fromIntegral . uint . least 1 . toBV 

unpack :: Uint256 -> [Word8]
unpack = map fromIntegral . split 32 . toBV
