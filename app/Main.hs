{-# LANGUAGE OverloadedStrings #-}

module Main where

import Prelude hiding (EQ, LT, GT, id)
import Ast

import Crypto.Hash.Keccak (keccak256)
import qualified Data.ByteArray as BA
import qualified Env
import qualified Memory
import BytecodeDecode (decode)

main :: IO ()
main = 
    let s = State
            { callState = CallState 
                { id = 0
                , caller = 1
                , callValue = 0
                , callData = BA.pack . BA.unpack $ ((BA.take 4 $ keccak256 "get(uint256)") `BA.append` Memory.fromWord 323 )}
            , program = Program $ decode exampleByteCode
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

exampleByteCode :: String
exampleByteCode = "608060405234801561000f575f80fd5b5060043610610034575f3560e01c806360fe47b1146100385780636d4ce63c14610054575b5f80fd5b610052600480360381019061004d91906101cc565b610072565b005b61005c61018c565b6040516100699190610206565b60405180910390f35b5f8054906101000a900473ffffffffffffffffffffffffffffffffffffffff1673ffffffffffffffffffffffffffffffffffffffff166323b872dd3330846040518463ffffffff1660e01b81526004016100ce9392919061025e565b6020604051808303815f875af11580156100ea573d5f803e3d5ffd5b505050506040513d601f19601f8201168201806040525081019061010e91906102c8565b505f3373ffffffffffffffffffffffffffffffffffffffff16600360405161013590610320565b5f6040518083038185875af1925050503d805f811461016f576040519150601f19603f3d011682016040523d82523d5f602084013e610174565b606091505b5050905080610181575f80fd5b816001819055505050565b5f600154905090565b5f80fd5b5f819050919050565b6101ab81610199565b81146101b5575f80fd5b50565b5f813590506101c6816101a2565b92915050565b5f602082840312156101e1576101e0610195565b5b5f6101ee848285016101b8565b91505092915050565b61020081610199565b82525050565b5f6020820190506102195f8301846101f7565b92915050565b5f73ffffffffffffffffffffffffffffffffffffffff82169050919050565b5f6102488261021f565b9050919050565b6102588161023e565b82525050565b5f6060820190506102715f83018661024f565b61027e602083018561024f565b61028b60408301846101f7565b949350505050565b5f8115159050919050565b6102a781610293565b81146102b1575f80fd5b50565b5f815190506102c28161029e565b92915050565b5f602082840312156102dd576102dc610195565b5b5f6102ea848285016102b4565b91505092915050565b5f81905092915050565b50565b5f61030b5f836102f3565b9150610316826102fd565b5f82019050919050565b5f61032a82610300565b915081905091905056fea264697066735822122086d3c84c269a1ad0a7ca5cec1b01ee530c5b5a85ba6eb0623eb0b35a4323bea964736f6c63430008150033"

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
