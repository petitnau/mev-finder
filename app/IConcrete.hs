module IConcrete where

data Call =
    Call Uint256 String [(Type, Uint256)] Uint256
    deriving Show

callId :: Call -> BA.Bytes
callId (Call _ fn args _) =
    (encode . keccak256 . S.fromString $ fn ++ "(" ++ (L.intercalate "," . L.map (typeId . fst) $ args) ++ ")")
    `BA.append` BA.concat (L.map (Memory.fromUint256 . snd) args :: [BA.Bytes])
    where encode = BA.pack . BA.unpack . BA.take 4

