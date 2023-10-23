{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE BlockArguments #-}
module Utils where

import Data.Bits
import Control.Monad (join, liftM)
import Data.Functor ((<&>))
import Data.String.HT (trim)
import qualified Data.ByteArray as BA
import Crypto.Hash.Keccak (keccak256)
import qualified Data.String as S
import qualified Data.Binary.Get as B
import qualified Data.ByteString.Lazy.Internal as BS
import Debug.Trace (traceShowId, trace)
import Data.Bifunctor (Bifunctor(bimap))
import Protolude ((<<$>>))
import Protolude.Applicative ((<<*>>))

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

(>>>>=) :: (Monad m1, Monad m2, Monad m3, Traversable m2, Traversable m3) => m1 (m2 (m3 a)) -> (a -> m1 (m2 (m3 b))) -> m1 (m2 (m3 b))
(>>>>=) m f = m >>= traverse (traverse f) >>= (\x -> return $ x >>= (fmap join . sequenceA))

(<<<$>>>) :: (Functor f, Functor g, Functor h) => (a -> b) -> f (g (h a)) -> f (g (h b))
(<<<$>>>) = fmap . fmap . fmap

bimap' :: (a -> b) -> (a,a) -> (b,b)
bimap' f (x, y) = (f x, f y)

trimap' :: (a -> b) -> (a,a,a) -> (b,b,b)
trimap' f (x, y, z) = (f x, f y, f z)

foldbi :: (a -> a -> a) -> (a,a) -> a
foldbi f (x, y) = x `f` y

foldtri :: (a -> a -> a) -> (a,a,a) -> a
foldtri f (x, y, z) = x `f` y `f` z

orM :: Monad m => m Bool -> m Bool -> m Bool
orM m1 m2 = m1 >>= \x -> if x then return True else m2

anyM :: (Foldable t, Monad m) => (a -> m Bool) -> t a -> m Bool
anyM f = foldr (orM . f) (pure False)

bytesToInteger :: BA.Bytes -> Integer
bytesToInteger = toInteger . B.runGet B.getWord32be . BS.packBytes . BA.unpack

funId :: String -> BA.Bytes
funId = BA.pack . BA.unpack . BA.take 4 . keccak256 . S.fromString

(<<) :: (Monad m) => m a -> m () -> m a
(<<) m n = m >>= \x -> do n >> return x

trimap :: (a -> d) -> (b -> e) -> (c -> f) -> (a,b,c) -> (d,e,f)
trimap f g h (a,b,c) = (f a, g b, h c)


scanM :: (Monad m) => (b -> a -> m b) -> b -> [a] -> m [b]
scanM _ z [] = return []
scanM f z (x:xs) = do
  z' <- f z x
  r <- scanM f z' xs
  return $ z':r

(<.>) :: Functor m => (b -> c) -> (a -> m b) -> a -> m c
(f <.> g) a = f <$> g a

iterateBoundedM :: Monad m => Integer -> (a -> m a) -> a -> m [a]
iterateBoundedM 0 f x = return [x]
iterateBoundedM n f x = do
    x' <- f x
    (x:) `liftM` iterateBoundedM (n-1) f x'

chunksOf :: Int -> [e] -> [[e]]
chunksOf i ls = map (take i) (splitter ls (:) [])
 where
  splitter :: [e] -> ([e] -> a -> a) -> a -> a
  splitter [] _ n = n
  splitter l c n = l `c` splitter (drop i l) c n

biliftA2 :: (a -> b -> c) -> (d -> e -> f) -> (a, d) -> (b, e) -> (c, f)
biliftA2 f g (a,d) (b,e) = (f a b, g d e)
