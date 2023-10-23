{-# LANGUAGE BlockArguments #-}
module Main where

import Test.Tasty
import Test.Tasty.HUnit
import qualified System.Exit as Exit
import Symbolic.SMT (word, (+++), Expr)
import Symbolic.Interface (buildAxioms, checkMEV, StorageKey (SPos, SMap))
import Solidity.BytecodeDecode (decodeProgram)
import Ast (Program)
import Data.Functor ((<&>))
import Concrete.Interface (Type(Tbytes32, TUint256), Call (Call))

openProgram :: String -> IO Program
openProgram fn = do
    decodeProgram <$> readFile ("examples/bin/" ++ fn ++ ".bin-runtime")

eth, eur, dkk :: Integer
eth = 0
eur = 1
dkk = 2

contract, mallory, alice, bob :: Integer
contract = 0
mallory = 1
alice = 2
bob = 3

main :: IO ()
main = defaultMain $ testGroup "Tests"

        [ testGroup "Withdrawer"

            [ testCase "base" do
                let attackers = [mallory]
                let worth = [(eth, 1)]
                let balances =
                        [ ((eth, contract), 500) 
                        , ((eth, mallory),  0) ]
                let allowances = []
                let storage = [] 
                res <- openProgram "Withdrawer" >>= checkMEV 1 attackers [] worth balances allowances storage
                res @?= 500 ]

        , testGroup "CoinPusher"

            [ testCase "Push" do
                let attackers = [mallory]
                let worth = [(eth, 1)]
                let balances =
                        [ ((eth, contract), 10) 
                        , ((eth, mallory),  200) ]
                let allowances = []
                let storage = [] 
                res <- openProgram "CoinPusher" >>= checkMEV 1 attackers [] worth balances allowances storage
                res @?= 10
            
            , testCase "No Push" do
                let attackers = [mallory]
                let worth = [(eth, 1)]
                let balances =
                        [ ((eth, contract), 10) 
                        , ((eth, mallory),  89) ]
                let allowances = []
                let storage = [] 
                res <- openProgram "CoinPusher" >>= checkMEV 1 attackers [] worth balances allowances storage
                res @?= 0 ]

        , testGroup "BoundedWithdrawer"

            [ testCase "One txn" do
                let attackers = [mallory]
                let worth = [(eth, 1)]
                let balances =
                        [ ((eth, contract), 25) 
                        , ((eth, mallory),  200) ]
                let allowances = []
                let storage = [] 
                res <- openProgram "BoundedWithdrawer" >>= checkMEV 1 attackers [] worth balances allowances storage
                res @?= 10

            , testCase "Two txn" do
                let attackers = [mallory]
                let worth = [(eth, 1)]
                let balances =
                        [ ((eth, contract), 25) 
                        , ((eth, mallory),  200) ]
                let allowances = []
                let storage = [] 
                res <- openProgram "BoundedWithdrawer" >>= checkMEV 2 attackers [] worth balances allowances storage
                res @?= 20

            ,  testCase "Three txn" do
                let attackers = [mallory]
                let worth = [(eth, 1)]
                let balances =
                        [ ((eth, contract), 25) 
                        , ((eth, mallory),  200) ]
                let allowances = []
                let storage = [] 
                res <- openProgram "BoundedWithdrawer" >>= checkMEV 3 attackers [] worth balances allowances storage
                res @?= 25 ]

        , testGroup "Whitelist"

            [ testCase "Whitelisted" do
                let attackers = [mallory]
                let worth = [(eth, 1)]
                let balances =
                        [ ((eth, contract), 25) 
                        , ((eth, mallory),  200) ]
                let allowances = []
                let storage = [] 
                res <- openProgram "Whitelist" >>= checkMEV 1 attackers [] worth balances allowances storage
                res @?= 25

            , testCase "Not whitelisted" do
                let attackers = [alice]
                let worth = [(eth, 1)]
                let balances =
                        [ ((eth, contract), 25) 
                        , ((eth, alice),    200) ]
                let allowances = []
                let storage = [] 
                res <- openProgram "Whitelist" >>= checkMEV 1 attackers [] worth balances allowances storage
                res @?= 0 ]

        , testGroup "Blacklist"

            [ testCase "Blacklisted" do
                let attackers = [mallory]
                let worth = [(eth, 1)]
                let balances =
                        [ ((eth, contract), 25) 
                        , ((eth, mallory),  200) ]
                let allowances = []
                let storage = [] 
                res <- openProgram "Blacklist" >>= checkMEV 1 attackers [] worth balances allowances storage
                res @?= 0

            , testCase "Not blacklisted" do
                let attackers = [alice]
                let worth = [(eth, 1)]
                let balances =
                        [ ((eth, contract), 25) 
                        , ((eth, alice),    200) ]
                let allowances = []
                let storage = [] 
                res <- openProgram "Blacklist" >>= checkMEV 1 attackers [] worth balances allowances storage
                res @?= 25

            , testCase "Some blacklisted" do
                let attackers = [alice, mallory]
                let worth = [(eth, 1)]
                let balances =
                        [ ((eth, contract), 25) 
                        , ((eth, alice),    200)
                        , ((eth, mallory),  200) ]
                let allowances = []
                let storage = [] 
                res <- openProgram "Blacklist" >>= checkMEV 1 attackers [] worth balances allowances storage
                res @?= 25 ]
        
        , testGroup "TokenWithdrawer"

            [ testCase "One txn" do
                let attackers = [alice]
                let worth = [(eur, 5)]
                let balances =
                        [ ((eur, contract), 2) 
                        , ((eur, alice),    100) ]
                let allowances = []
                let storage = [] 
                res <- openProgram "TokenWithdrawer" >>= checkMEV 1 attackers [] worth balances allowances storage
                res @?= 5
                
            , testCase "Two txn" do
                let attackers = [alice]
                let worth = [(eur, 5)]
                let balances =
                        [ ((eur, contract), 2)
                        , ((eur, alice),    100)  ]
                let allowances = []
                let storage = [] 
                res <- openProgram "TokenWithdrawer" >>= checkMEV 2 attackers [] worth balances allowances storage
                res @?= 10

            , testCase "Three txn" do
                let attackers = [alice]
                let worth = [(eur, 5)]
                let balances =
                        [ ((eur, contract), 2)
                        , ((eur, alice),    100)  ]
                let allowances = []
                let storage = [] 
                res <- openProgram "TokenWithdrawer" >>= checkMEV 3 attackers [] worth balances allowances storage
                res @?= 10 ]
        
        {-, testGroup "AMM"

            [ testCase "Arbitrage1" do
                let attackers = [mallory]
                let worth = [(eur, 5), (dkk, 1)]
                let balances =
                        [ ((eth, contract), 500)
                        , ((eur, contract), 500)
                        , ((dkk, contract), 500)
                        , ((eth, mallory), 500)
                        , ((eur, mallory), 500)
                        , ((dkk, mallory), 500) ]
                let allowances = 
                        [ ((eur, contract, mallory), 500) 
                        , ((dkk, contract, mallory), 500) ]
                let storage = 
                        [ (SPos 0, 500) 
                        , (SPos 1, 500) ]
                res <- openProgram "AMM" >>= checkMEV 1 attackers [] worth balances allowances storage
                assertEqual "" 2000 res 

            , testCase "Arbitrage2" do
                let attackers = [mallory]
                let worth = [(eur, 5), (dkk, 1)]
                let balances =
                        [ ((eth, contract), 1500)
                        , ((eur, contract), 1500)
                        , ((dkk, contract), 1500)
                        , ((eth, mallory),  500)
                        , ((eur, mallory),  500)
                        , ((dkk, mallory),  500)
                        , ((eth, alice),    500)
                        , ((eur, alice),    500)
                        , ((dkk, alice),    500) ]
                let allowances = 
                        [ ((eur, contract, mallory), 500) 
                        , ((dkk, contract, mallory), 500) 
                        , ((eur, contract, alice),   500) 
                        , ((dkk, contract, alice),   500)]
                let storage = 
                        [ (SPos 0, 1500) 
                        , (SPos 1, 1500) ]
                let mempool = 
                        [Call (fromInteger alice) "swap0" [(TUint256, 500)] 0]
                res <- openProgram "AMM" >>= checkMEV 2 attackers mempool worth balances allowances storage
                assertEqual "" 2000 res 

            , testCase "Sandwich" do
                let attackers = [mallory]
                let worth = [(eur, 1), (dkk, 1)]
                let balances =
                        [ ((eth, contract), 1500)
                        , ((eur, contract), 1500)
                        , ((dkk, contract), 1500)
                        , ((eth, mallory),  500)
                        , ((eur, mallory),  500)
                        , ((dkk, mallory),  500)
                        , ((eth, alice),    500)
                        , ((eur, alice),    500)
                        , ((dkk, alice),    500) ]
                let allowances = 
                        [ ((eur, contract, mallory), 500) 
                        , ((dkk, contract, mallory), 500) 
                        , ((eur, contract, alice),   500) 
                        , ((dkk, contract, alice),   500)]
                let storage = 
                        [ (SPos 0, 500) 
                        , (SPos 1, 500) ]
                let mempool = 
                        [Call (fromInteger alice) "swap0" [(TUint256, 500)] 0]
                res <- openProgram "AMM" >>= checkMEV 2 attackers mempool worth balances allowances storage
                assertEqual "" 2000 res 
                
        , testCase "Fake (5min)" do
                let attackers = [mallory]
                let worth = [(eth, 1)]
                let balances =
                        [ ((eth, contract), 5000)
                        , ((eth, mallory),  400)
                        , ((eth, alice),  500)]
                let allowances = 
                        [  ]
                let storage = 
                        [ (SPos 0, 1500) 
                        , (SPos 1, 1500) ]
                let mempool = [Call (fromInteger alice) "swap0" [] 500]
                res <- openProgram "FakeAMM" >>= checkMEV 2 attackers mempool worth balances allowances storage
                assertEqual "" 124 res ] 
                -}

        , testGroup "HashLock"

            [ testCase "No Mempool" do
                let attackers = [mallory]
                let worth = [(eth, 1)]
                let balances =
                        [ ((eth, contract), 25) 
                        , ((eth, mallory),  200) ]
                let allowances = []
                let storage = 
                        [ (SPos 0, 0xbd93a12ad13e81e9f6d5854d1e6163aae29ab3a1e1aae46441415b6338156780) ] 
                res <- openProgram "HashLock" >>= checkMEV 1 attackers [] worth balances allowances storage
                res @?= 0

            , testCase "Mempool one txn" do
                let attackers = [mallory]
                let worth = [(eth, 1)]
                let balances =
                        [ ((eth, contract), 25) ]
                let allowances = 
                        [ ((eur, contract, mallory), 0) ]
                let storage =  
                        [ (SPos 0, 0xbd93a12ad13e81e9f6d5854d1e6163aae29ab3a1e1aae46441415b6338156780) ] 
                let mempool = 
                        [Call (fromInteger alice) "withdraw" [(Tbytes32, 0x90722a86d157231f39e98219c0c6084bbf7ca4b33e924dd3bdec1864ee8d4af8)] 0]
                res <- openProgram "HashLock" >>= checkMEV 1 attackers mempool worth balances allowances storage
                res @?= 10

            , testCase "Mempool two txn" do

                let attackers = [mallory]
                let worth = [(eth, 1)]
                let balances =
                        [ ((eth, contract), 25)
                        , ((eth, mallory),  200) ]
                let allowances = 
                        [ ((eur, contract, mallory), 0) ]
                let storage =  
                        [ (SPos 0, 0xbd93a12ad13e81e9f6d5854d1e6163aae29ab3a1e1aae46441415b6338156780) ] 
                let mempool = 
                        [Call (fromInteger alice) "withdraw" [(Tbytes32, 0x90722a86d157231f39e98219c0c6084bbf7ca4b33e924dd3bdec1864ee8d4af8)] 0]
                res <- openProgram "HashLock" >>= checkMEV 2 attackers mempool worth balances allowances storage
                assertEqual "" 20 res ]
        
        , testGroup "Allower"

            [ testCase "Alice one txn" do
                let attackers = [mallory]
                let worth = [(eth, 1)]
                let balances =
                        [ ((eth, contract), 25) 
                        , ((eth, mallory),  200) ]
                let allowances = []
                let storage = 
                        [ (SPos 0, 0)
                        , (SPos 1, alice) ]
                let mempool = 
                        [Call (fromInteger alice) "setAllow" [] 0]

                res <- openProgram "Allower" >>= checkMEV 1 attackers mempool worth balances allowances storage
                res @?= 0

            , testCase "Alice two txn" do
                let attackers = [mallory]
                let worth = [(eth, 1)]
                let balances =
                        [ ((eth, contract), 25) 
                        , ((eth, mallory),  200) ]
                let allowances = []
                let storage = 
                        [ (SPos 0, 0)
                        , (SPos 1, alice) ]
                let mempool = 
                        [Call (fromInteger alice) "setAllow" [] 0]

                res <- openProgram "Allower" >>= checkMEV 2 attackers mempool worth balances allowances storage
                res @?= 25

            , testCase "Bob two txn" do
                let attackers = [mallory]
                let worth = [(eth, 1)]
                let balances =
                        [ ((eth, contract), 25) 
                        , ((eth, mallory),  200) ]
                let allowances = []
                let storage = 
                        [ (SPos 0, 0)
                        , (SPos 1, alice) ]
                let mempool = 
                        [Call (fromInteger bob) "setAllow" [] 0]

                res <- openProgram "Allower" >>= checkMEV 2 attackers mempool worth balances allowances storage
                res @?= 0 ]
        {-
        , testGroup "Bank" 

            [ testCase "Withdraw" do
                let attackers = [alice]
                let worth = [(eth, 1)]
                let balances =
                        [ ((eth, contract), 200) 
                        , ((eth, alice),  200) ]
                let allowances = []
                let storage = 
                        [ (SMap (SPos 0) alice, 20) ]
                res <- openProgram "Bank" >>= checkMEV 1 attackers [] worth balances allowances storage
                res @?= 20 ]
        -}
        ]
 