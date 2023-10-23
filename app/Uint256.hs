module Uint256 where

import Data.DoubleWord
import Data.Bits
import Utils
import Data.Word (Word8)

data Uint256 = Uint256 Word256
    deriving (Eq, Ord)

instance Show Uint256 where
    show (Uint256 a) = show a

instance Num Uint256 where
    (+) (Uint256 a) (Uint256 b) = Uint256 $ a+b
    (*) (Uint256 a) (Uint256 b) = Uint256 $ a*b
    (-) (Uint256 a) (Uint256 b) = Uint256 $ a-b
    fromInteger n = Uint256 $ fromInteger n
    
    signum (Uint256 a) 
        | a == 0        = 0
        | a < (2^255)   = 1
        | otherwise     = -1
    abs x
        | signum x == -1    = 0 - x
        | signum x == 1     = x
        | otherwise         = 0

instance Real Uint256 where
    toRational (Uint256 a) = toRational a

instance Enum Uint256 where
    fromEnum (Uint256 a) = fromEnum a
    toEnum n = Uint256 $ toEnum n

instance Integral Uint256 where
    toInteger (Uint256 a) = toInteger a
    quotRem _ 0 = (0, 0)
    quotRem (Uint256 a) (Uint256 b) =
        let (q,r) = quotRem a b in
        (Uint256 q, Uint256 r)
    divMod (Uint256 a) (Uint256 b) =
        let (d,m) = divMod a b in
        (Uint256 d, Uint256 m)

instance Bits Uint256 where
    (.&.) (Uint256 a) (Uint256 b) = Uint256 $ a .&. b 
    (.|.) (Uint256 a) (Uint256 b) = Uint256 $ a .|. b 
    xor (Uint256 a) (Uint256 b) = Uint256 $ a `xor` b 
    complement (Uint256 a) = Uint256 $ complement a 
    shift (Uint256 a) n = Uint256 $ shift a n 
    rotate (Uint256 a) n = Uint256 $ rotate a n 
    bitSize (Uint256 a) = finiteBitSize a
    bitSizeMaybe (Uint256 a) = bitSizeMaybe a
    isSigned _ = True
    testBit (Uint256 a) i = testBit a i
    bit i = Uint256 $ bit i
    popCount (Uint256 a) = popCount a

(/+) :: Uint256 -> Uint256 -> Uint256
(/+) a b 
    | (signum a) * (signum b) == 1  = (abs a) `div` (abs b) 
    | (signum a) * (signum b) == -1 = 0 - ((abs a) `div` (abs b))
    
(%+) :: Uint256 -> Uint256 -> Uint256
(%+) a b 
    | (signum a) == 1  = (abs a) `mod` (abs b) 
    | (signum a) == -1 = 0 - ((abs a) `mod` (abs b))


(<+) :: Uint256 -> Uint256 -> Bool
(<+) a b 
    | (signum a) == 1 && (signum b) == 1    = a < b
    | (signum a) == 1 && (signum b) == -1   = False
    | (signum a) == -1 && (signum b) == 1   = True
    | (signum a) == -1 && (signum b) == -1  = abs b < abs a

(^%) :: Uint256 -> Uint256 -> Uint256
(^%) a b = fromInteger $ modExp (toInteger a) (toInteger b) (2^256)

toBool :: Uint256 -> Bool
toBool 0 = False
toBool _ = True

fromBool :: Bool -> Uint256
fromBool False = 0
fromBool True = 1

lowestByte :: Uint256 -> Word8
lowestByte (Uint256 w) = loWord . loWord . loWord . loWord . loWord $ w 
