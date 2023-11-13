module Concrete.Utils where

import Crypto.Hash.Keccak (keccak256)
import qualified Data.String as S
import qualified Data.List as L
import qualified Data.ByteArray as BA

funId :: String -> BA.Bytes
funId = BA.pack . BA.unpack . BA.take 4 . keccak256 . S.fromString
