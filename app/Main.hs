{-# LANGUAGE OverloadedStrings #-}

module Main where

import Prelude hiding (EQ, LT, GT, id)
import Ast

import Crypto.Hash.Keccak (keccak256)
import qualified Data.ByteArray as BA
import qualified Env
import qualified Memory

main :: IO ()
main = 
    let s = State
            { callState = CallState 
                { id = 0
                , caller = 1
                , callValue = 0
                , callData = BA.pack . BA.unpack $ ((BA.take 4 $ keccak256 "get(uint256)") `BA.append` Memory.fromWord 323 )}
            , program = ast
            , pc = 0
            , origin = 1
            , block = BlockInfo
                { number = 0
                , timestamp = 0
                , balances = Env.empty `Env.bind` (0,0) `Env.bind` (0,1) }
            , stack = []
            , memory = BA.empty
            , storage = Env.empty }
    in print $ sem s

ast :: Program
ast = Program
    [ PUSH 1     0x80
    , PUSH 1     0x40
    , MSTORE    
    , CALLVALUE 
    , DUP 1      
    , ISZERO    
    , PUSH 2     0xf
    , JUMPI     
    , PUSH 0     0x0     
    , DUP 1      
    , REVERT    
    , JUMPDEST  
    , POP       
    , PUSH 1     0x4
    , CALLDATASIZE
    , LT        
    , PUSH 2     0x29
    , JUMPI     
    , PUSH 0     0x0     
    , CALLDATALOAD
    , PUSH 1     0xe0
    , SHR       
    , DUP 1      
    , PUSH 4     0x9507d39a
    , EQ        
    , PUSH 2     0x2d
    , JUMPI     
    , JUMPDEST  
    , PUSH 0     0x0     
    , DUP 1      
    , REVERT    
    , JUMPDEST  
    , PUSH 2     0x47
    , PUSH 1     0x4
    , DUP 1      
    , CALLDATASIZE
    , SUB       
    , DUP 2      
    , ADD       
    , SWAP 1     
    , PUSH 2     0x42
    , SWAP 2     
    , SWAP 1     
    , PUSH 2     0xa9
    , JUMP      
    , JUMPDEST  
    , PUSH 2     0x5d
    , JUMP      
    , JUMPDEST  
    , PUSH 1     0x40
    , MLOAD     
    , PUSH 2     0x54
    , SWAP 2     
    , SWAP 1     
    , PUSH 2     0xe3
    , JUMP      
    , JUMPDEST  
    , PUSH 1     0x40
    , MLOAD     
    , DUP 1      
    , SWAP 2     
    , SUB       
    , SWAP 1     
    , RETURN    
    , JUMPDEST  
    , PUSH 0     0x0     
    , PUSH 1     0x5
    , DUP 3      
    , PUSH 2     0x6b
    , SWAP 2     
    , SWAP 1     
    , PUSH 2     0x129
    , JUMP      
    , JUMPDEST  
    , SWAP 1     
    , POP       
    , SWAP 2     
    , SWAP 1     
    , POP       
    , JUMP      
    , JUMPDEST  
    , PUSH 0     0x0     
    , DUP 1      
    , REVERT    
    , JUMPDEST  
    , PUSH 0     0x0     
    , DUP 2      
    , SWAP 1     
    , POP       
    , SWAP 2     
    , SWAP 1     
    , POP       
    , JUMP      
    , JUMPDEST  
    , PUSH 2     0x88
    , DUP 2      
    , PUSH 2     0x76
    , JUMP      
    , JUMPDEST  
    , DUP 2      
    , EQ        
    , PUSH 2     0x92
    , JUMPI     
    , PUSH 0     0x0     
    , DUP 1      
    , REVERT    
    , JUMPDEST  
    , POP       
    , JUMP      
    , JUMPDEST  
    , PUSH 0     0x0     
    , DUP 2      
    , CALLDATALOAD
    , SWAP 1     
    , POP       
    , PUSH 2     0xa3
    , DUP 2      
    , PUSH 2     0x7f
    , JUMP      
    , JUMPDEST  
    , SWAP 3     
    , SWAP 2     
    , POP       
    , POP       
    , JUMP      
    , JUMPDEST  
    , PUSH 0     0x0     
    , PUSH 1     0x20
    , DUP 3      
    , DUP 5      
    , SUB       
    , SLT       
    , ISZERO    
    , PUSH 2     0xbe
    , JUMPI     
    , PUSH 2     0xbd
    , PUSH 2     0x72
    , JUMP      
    , JUMPDEST  
    , JUMPDEST  
    , PUSH 0     0x0     
    , PUSH 2     0xcb
    , DUP 5      
    , DUP 3      
    , DUP 6      
    , ADD       
    , PUSH 2     0x95
    , JUMP      
    , JUMPDEST  
    , SWAP 2     
    , POP       
    , POP       
    , SWAP 3     
    , SWAP 2     
    , POP       
    , POP       
    , JUMP      
    , JUMPDEST  
    , PUSH 2     0xdd
    , DUP 2      
    , PUSH 2     0x76
    , JUMP      
    , JUMPDEST  
    , DUP 3      
    , MSTORE    
    , POP       
    , POP       
    , JUMP      
    , JUMPDEST  
    , PUSH 0     0x0     
    , PUSH 1     0x20
    , DUP 3      
    , ADD       
    , SWAP 1     
    , POP       
    , PUSH 2     0xf6
    , PUSH 0     0x0     
    , DUP 4      
    , ADD       
    , DUP 5      
    , PUSH 2     0xd4
    , JUMP      
    , JUMPDEST  
    , SWAP 3     
    , SWAP 2     
    , POP       
    , POP       
    , JUMP      
    , JUMPDEST  
    , PUSH 32    0x4e487b7100000000000000000000000000000000000000000000000000000000
    , PUSH 0     0x0     
    , MSTORE    
    , PUSH 1     0x11
    , PUSH 1     0x4
    , MSTORE    
    , PUSH 1     0x24
    , PUSH 0     0x0     
    , REVERT    
    , JUMPDEST  
    , PUSH 0     0x0     
    , PUSH 2     0x133
    , DUP 3      
    , PUSH 2     0x76
    , JUMP      
    , JUMPDEST  
    , SWAP 2     
    , POP       
    , PUSH 2     0x13e
    , DUP 4      
    , PUSH 2     0x76
    , JUMP      
    , JUMPDEST  
    , SWAP 3     
    , POP       
    , DUP 3      
    , DUP 3      
    , ADD       
    , SWAP 1     
    , POP       
    , DUP 1      
    , DUP 3      
    , GT        
    , ISZERO    
    , PUSH 2     0x156
    , JUMPI     
    , PUSH 2     0x155
    , PUSH 2     0xfc
    , JUMP      
    , JUMPDEST  
    , JUMPDEST  
    , SWAP 3     
    , SWAP 2     
    , POP       
    , POP       
    , JUMP      
    , REVERT   
    , LOG 2      
    , PUSH 5     0x6970667358
    , REVERT   
    , SLT       
    , SHA3      
    , REVERT   
    , LOG 3      
    , SELFDESTRUCT
    , REVERT   
    , REVERT   
    , SWAP 2     
    , SWAP 14    
    , PUSH 29    0x60637e12b12ec0871c14af0fe2321983647a472638a10dff64736f6c63
    , NUMBER    
    , STOP      
    , ADDMOD    
    , ISZERO    
    , STOP      
    , CALLER ]
