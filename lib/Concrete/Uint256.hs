{-# LANGUAGE DataKinds #-}

module Concrete.Uint256 where

import Data.DoubleWord
import Data.Bits
import Utils
import Data.Word (Word8)
import Data.BitVector.Sized as BV
import Data.Maybe (fromJust)
import GHC.Natural (Natural)
import Data.Tuple.HT
import Debug.Trace

newtype Uint256 = Uint256 { toBV :: BV 256 }
    deriving (Eq, Ord)

instance Show Uint256 where
    show (Uint256 a) = show a

apply1 :: (BV 256 -> BV 256) -> Uint256 -> Uint256
apply1 f = Uint256 . f . toBV

apply1_ :: (BV 256 -> r) -> Uint256 -> r
apply1_ f = f . toBV

apply2 :: (BV 256 -> BV 256 -> BV 256) -> Uint256 -> Uint256 -> Uint256
apply2 f = curry $ Uint256 . uncurry f . mapPairSame toBV

apply2f :: (BV 256 -> b -> BV 256) -> Uint256 -> b -> Uint256
apply2f f = curry $ Uint256 . uncurry f . mapFst toBV

apply2_ :: (BV 256 -> BV 256 -> r) -> Uint256 -> Uint256 -> r
apply2_ f = curry $  uncurry f . mapPairSame toBV

bvadd, bvmul, bvsub, bvuquot, bvsquot, bvurem, bvsrem :: Uint256 -> Uint256 -> Uint256
bvadd   = apply2 (BV.add   knownNat)
bvmul   = apply2 (BV.mul   knownNat)
bvsub   = apply2 (BV.sub   knownNat)
bvuquot = apply2  BV.uquot
bvsquot = apply2 (BV.squot knownNat)
bvurem  = apply2  BV.urem
bvsrem  = apply2 (BV.srem  knownNat)

bvult, bvugt, bvslt, bvsgt :: Uint256 -> Uint256 -> Bool
bvult = apply2_ BV.ult
bvugt = flip bvult
bvslt = apply2_ (BV.slt knownNat)
bvsgt = flip bvslt

bvand, bvor, bvxor :: Uint256 -> Uint256 -> Uint256
bvand = apply2 BV.and
bvor  = apply2 BV.or
bvxor = apply2 BV.xor

bvshl, bvlshr, bvashr :: Uint256 -> Natural -> Uint256
bvshl  = apply2f (BV.shl knownNat)
bvlshr = apply2f (BV.lshr knownNat)
bvashr = apply2f (BV.ashr knownNat)

bvabs, bvsignum, bvcomplement :: Uint256 -> Uint256
bvabs        = apply1 (BV.abs knownNat)
bvsignum     = apply1 (BV.signum knownNat)
bvcomplement = apply1 (BV.complement knownNat)

instance Num Uint256 where
    (+) = bvadd
    (*) = bvmul
    abs = bvabs
    signum = bvsignum
    fromInteger = Uint256 . fromJust . mkBVUnsigned knownNat 
    (-) = bvsub

toBool :: Uint256 -> Bool
toBool (Uint256 x) = x /= zero knownNat

fromBool :: Bool -> Uint256
fromBool False = Uint256 $ zero knownNat
fromBool True = Uint256 $ one knownNat

touInteger :: Uint256 -> Integer
touInteger = BV.asUnsigned . toBV 

touInt:: Uint256 -> Int
touInt = fromIntegral . touInteger

toWord256 :: Uint256 -> Word256
toWord256 = fromIntegral . touInteger


lbyte :: Uint256 -> Word8
lbyte = head . fromJust . BV.asBytesLE knownNat . toBV

toNatural :: Uint256 -> Natural
toNatural = BV.asNatural . toBV
