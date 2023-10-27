module BytecodeDecode where

import Ast
import Uint256
import qualified Text.Hex as TH
import qualified Data.Text as Text
import qualified Data.ByteString as BS
import qualified Data.ByteArray as BA
import Data.Maybe (fromJust)
import Memory (toWord)
import Prelude hiding (EQ, LT, GT, id)

padToWordSize :: String -> String
padToWordSize s
    | length s <  64  = padToWordSize $ '0' : '0' :  s
    | otherwise = s

createWord :: String -> Uint256
createWord = (toWord :: BA.Bytes -> Uint256) . BA.pack . BS.unpack . fromJust . 
             TH.decodeHex . Text.pack . padToWordSize

decode :: String -> [Ast]
decode ('0': '0': r) = STOP          : decode r
decode ('0': '1': r) = ADD           : decode r
decode ('0': '2': r) = MUL           : decode r
decode ('0': '3': r) = SUB           : decode r
decode ('0': '4': r) = DIV           : decode r
decode ('0': '5': r) = SDIV          : decode r
decode ('0': '6': r) = MOD           : decode r
decode ('0': '7': r) = SMOD          : decode r
decode ('0': '8': r) = ADDMOD        : decode r
decode ('0': '9': r) = MULMOD        : decode r
decode ('0': 'a': r) = EXP           : decode r
decode ('0': 'b': r) = SIGNEXTEND    : decode r
-- 0c-0f invalid instructions
decode ('1': '0': r) = LT            : decode r
decode ('1': '1': r) = GT            : decode r
decode ('1': '2': r) = SLT           : decode r
decode ('1': '3': r) = SGT           : decode r
decode ('1': '4': r) = EQ            : decode r
decode ('1': '5': r) = ISZERO        : decode r
decode ('1': '6': r) = AND           : decode r
decode ('1': '7': r) = OR            : decode r
decode ('1': '8': r) = XOR           : decode r
decode ('1': '9': r) = NOT           : decode r
decode ('1': 'a': r) = BYTE          : decode r
decode ('1': 'b': r) = SHL           : decode r
decode ('1': 'c': r) = SHR           : decode r
decode ('1': 'd': r) = SAR           : decode r
-- 1e-1f invalid instructions
decode ('2': '0': r) = SHA3          : decode r
-- 21-2f invalid instructions
decode ('3': '0': r) = ADDRESS       : decode r
decode ('3': '1': r) = BALANCE       : decode r
decode ('3': '2': r) = ORIGIN        : decode r
decode ('3': '3': r) = CALLER        : decode r
decode ('3': '4': r) = CALLVALUE     : decode r
decode ('3': '5': r) = CALLDATALOAD  : decode r
decode ('3': '6': r) = CALLDATASIZE  : decode r
decode ('3': '7': r) = CALLDATACOPY  : decode r
decode ('3': '8': r) = CODESIZE      : decode r
decode ('3': '9': r) = CODECOPY      : decode r
decode ('3': 'a': r) = GASPRICE      : decode r
decode ('3': 'b': r) = EXTCODESIZE   : decode r
decode ('3': 'c': r) = EXTCODECOPY   : decode r
decode ('3': 'd': r) = RETURNDATASIZE: decode r
decode ('3': 'e': r) = RETURNDATACOPY: decode r
decode ('3': 'f': r) = EXTCODEHASH   : decode r
decode ('4': '0': r) = BLOCKHASH     : decode r
decode ('4': '1': r) = COINBASE      : decode r
decode ('4': '2': r) = TIMESTAMP     : decode r
decode ('4': '3': r) = NUMBER        : decode r
decode ('4': '4': r) = PREVRANDAO    : decode r
decode ('4': '5': r) = GASLIMIT      : decode r
decode ('4': '6': r) = CHAINID       : decode r
decode ('4': '7': r) = SELFBALANCE   : decode r
decode ('4': '8': r) = BASEFEE       : decode r
-- 49-4f invalid instructions
decode ('5': '0': r) = POP           : decode r
decode ('5': '1': r) = MLOAD         : decode r
decode ('5': '2': r) = MSTORE        : decode r
decode ('5': '3': r) = MSTORE8       : decode r
decode ('5': '4': r) = SLOAD         : decode r
decode ('5': '5': r) = SSTORE        : decode r
decode ('5': '6': r) = JUMP          : decode r
decode ('5': '7': r) = JUMPI         : decode r
decode ('5': '8': r) = PC            : decode r
decode ('5': '9': r) = MSIZE         : decode r
decode ('5': 'a': r) = GAS           : decode r
decode ('5': 'b': r) = JUMPDEST      : decode r
-- 5c-5e invalid instructions
decode ('5': 'f': r) = PUSH 0  (createWord "")           : decode r
decode ('6': '0': r) = PUSH 1  (createWord $ take 2 r)   : decode (drop 2 r)
decode ('6': '1': r) = PUSH 2  (createWord $ take 4 r)   : decode (drop 4 r)
decode ('6': '2': r) = PUSH 3  (createWord $ take 6 r)   : decode (drop 6 r)
decode ('6': '3': r) = PUSH 4  (createWord $ take 8 r)   : decode (drop 8 r)
decode ('6': '4': r) = PUSH 5  (createWord $ take 10 r)  : decode (drop 10 r)
decode ('6': '5': r) = PUSH 6  (createWord $ take 12 r)  : decode (drop 12 r)
decode ('6': '6': r) = PUSH 7  (createWord $ take 14 r)  : decode (drop 14 r)
decode ('6': '7': r) = PUSH 8  (createWord $ take 16 r)  : decode (drop 16 r)
decode ('6': '8': r) = PUSH 9  (createWord $ take 18 r)  : decode (drop 18 r)
decode ('6': '9': r) = PUSH 10 (createWord $ take 20 r)  : decode (drop 20 r)
decode ('6': 'a': r) = PUSH 11 (createWord $ take 22 r)  : decode (drop 22 r)
decode ('6': 'b': r) = PUSH 12 (createWord $ take 24 r)  : decode (drop 24 r)
decode ('6': 'c': r) = PUSH 13 (createWord $ take 26 r)  : decode (drop 26 r)
decode ('6': 'd': r) = PUSH 14 (createWord $ take 28 r)  : decode (drop 28 r)
decode ('6': 'e': r) = PUSH 15 (createWord $ take 30 r)  : decode (drop 30 r)
decode ('6': 'f': r) = PUSH 16 (createWord $ take 32 r)  : decode (drop 32 r)
decode ('7': '0': r) = PUSH 17 (createWord $ take 34 r)  : decode (drop 34 r)
decode ('7': '1': r) = PUSH 18 (createWord $ take 36 r)  : decode (drop 36 r)
decode ('7': '2': r) = PUSH 19 (createWord $ take 38 r)  : decode (drop 38 r)
decode ('7': '3': r) = PUSH 20 (createWord $ take 40 r)  : decode (drop 40 r)
decode ('7': '4': r) = PUSH 21 (createWord $ take 42 r)  : decode (drop 42 r)
decode ('7': '5': r) = PUSH 22 (createWord $ take 44 r)  : decode (drop 44 r)
decode ('7': '6': r) = PUSH 23 (createWord $ take 46 r)  : decode (drop 46 r)
decode ('7': '7': r) = PUSH 24 (createWord $ take 48 r)  : decode (drop 48 r)
decode ('7': '8': r) = PUSH 25 (createWord $ take 50 r)  : decode (drop 50 r)
decode ('7': '9': r) = PUSH 26 (createWord $ take 52 r)  : decode (drop 52 r)
decode ('7': 'a': r) = PUSH 27 (createWord $ take 54 r)  : decode (drop 54 r)
decode ('7': 'b': r) = PUSH 28 (createWord $ take 56 r)  : decode (drop 56 r)
decode ('7': 'c': r) = PUSH 29 (createWord $ take 58 r)  : decode (drop 58 r)
decode ('7': 'd': r) = PUSH 30 (createWord $ take 60 r)  : decode (drop 60 r)
decode ('7': 'e': r) = PUSH 31 (createWord $ take 62 r)  : decode (drop 62 r)
decode ('7': 'f': r) = PUSH 32 (createWord $ take 64 r)  : decode (drop 64 r)
decode ('8': '0': r) = DUP 1          : decode r
decode ('8': '1': r) = DUP 2          : decode r
decode ('8': '2': r) = DUP 3          : decode r
decode ('8': '3': r) = DUP 4          : decode r
decode ('8': '4': r) = DUP 5          : decode r
decode ('8': '5': r) = DUP 6          : decode r
decode ('8': '6': r) = DUP 7          : decode r
decode ('8': '7': r) = DUP 8          : decode r
decode ('8': '8': r) = DUP 9          : decode r
decode ('8': '9': r) = DUP 10         : decode r
decode ('8': 'a': r) = DUP 11         : decode r
decode ('8': 'b': r) = DUP 12         : decode r
decode ('8': 'c': r) = DUP 13         : decode r
decode ('8': 'd': r) = DUP 14         : decode r
decode ('8': 'e': r) = DUP 15         : decode r
decode ('8': 'f': r) = DUP 16         : decode r
decode ('9': '0': r) = SWAP 1         : decode r
decode ('9': '1': r) = SWAP 2         : decode r
decode ('9': '2': r) = SWAP 3         : decode r
decode ('9': '3': r) = SWAP 4         : decode r
decode ('9': '4': r) = SWAP 5         : decode r
decode ('9': '5': r) = SWAP 6         : decode r
decode ('9': '6': r) = SWAP 7         : decode r
decode ('9': '7': r) = SWAP 8         : decode r
decode ('9': '8': r) = SWAP 9         : decode r
decode ('9': '9': r) = SWAP 10        : decode r
decode ('9': 'a': r) = SWAP 11        : decode r
decode ('9': 'b': r) = SWAP 12        : decode r
decode ('9': 'c': r) = SWAP 13        : decode r
decode ('9': 'd': r) = SWAP 14        : decode r
decode ('9': 'e': r) = SWAP 15        : decode r
decode ('9': 'f': r) = SWAP 16        : decode r
decode ('a': '0': r) = LOG 0          : decode r
decode ('a': '1': r) = LOG 1          : decode r
decode ('a': '2': r) = LOG 2          : decode r
decode ('a': '3': r) = LOG 3          : decode r
decode ('a': '4': r) = LOG 4          : decode r
-- a5-ef invalid instructions
decode ('f': '0': r) = CREATE        : decode r
decode ('f': '1': r) = CALL          : decode r
decode ('f': '2': r) = CALLCODE      : decode r
decode ('f': '3': r) = RETURN        : decode r
decode ('f': '4': r) = DELEGATECALL  : decode r
decode ('f': '5': r) = CREATE2       : decode r
-- f6-f9 invalid instructions
decode ('f': 'a': r) = STATICCALL    : decode r
-- fb-fc invalid instructions
decode ('f': 'd': r) = REVERT        : decode r
decode ('f': 'e': r) = INVALID       : decode r
decode ('f': 'f': r) = SELFDESTRUCT  : decode r
decode (_  :  _ : r) = REVERT        : decode r--matched an invalid instruction
decode "" = []