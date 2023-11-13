{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE BlockArguments #-}

module Symbolic.Interface where

import Ast
import GenericSem
import Symbolic.Sem
import Symbolic.SMT

import Prelude hiding (id)
import Data.Maybe (mapMaybe)
import Data.Tuple.Extra (thd3)
import Utils (anyM, orM)
import Control.Monad (forM, forM_, filterM)

baseState :: Program -> State
baseState prog = State
    { id = Var "Id"
    , caller = Var "Caller1"
    , callValue = Var "CallValue1"
    , callData = Var "CallData1"
    , number = Var "BlockNumber"
    , timestamp = Var "Timestamp"
    , program = prog
    , pc = 0
    , balances = Var "Balances"
    , stack = []
    , memory = mem 0
    , returned = mem 0
    , storage = Var "Storage"
    , extra = Extra {constraints = [], callDataSize = Var "CallDataSize1"} }

baseDecls :: [Decl]
baseDecls =
    [ DeclVar "Id" tword
    , DeclVar "BlockNumber" tword
    , DeclVar "Timestamp" tword
    , DeclVar "Balances" (TArray tword tword)
    , DeclVar "Storage" (TArray tword tword)
    ]

declsCall :: Integer -> [Decl]
declsCall i =
    [ DeclVar ("Caller" ++ show i) tword
    , DeclVar ("CallValue" ++ show i) tword
    , DeclVar ("CallDataSize" ++ show i) tword
    , DeclVar ("CallData" ++ show i) tmem ]

axiomsCall :: Integer -> [Expr]
axiomsCall i =
    [ Var ("CallData" ++ show i) `eq` (Var ("CallData" ++ show i) `bvand` ((mem 0 `bvsub` mem 1) `bvshl` (mem 1024 `bvsub` ((LBitVec 768 0 +++ Var ("CallDataSize" ++ show i)) `bvmul` mem 8))))
    , ForAll "x" tword ((Var "Balances" @! Var "x") `bvult` word 0x100000000000000000000000000000000) ]

check_ :: Integer -> State -> [Decl] -> [Expr] -> [Expr] -> ((Expr, State, Integer) -> [Expr]) -> ((Expr, State, Integer) -> [Expr]) -> (Integer -> [Expr]) -> Integer -> IO Bool
check_ 0 state decls memAxioms axioms checks maxims customAxioms i = return False
check_ depth state decls memAxioms axioms checks maxims customAxioms i = do
    putStrLn $ "Checking depth: " ++ show i
    let decls'  = decls  ++ declsCall i
    let axioms' = axioms ++ axiomsCall i
    let results = mapMaybe (\case Returned r -> Just r; _ -> Nothing) (sem state)
    print $ length results
    let constraintss2 = map (\(r,s) -> (r, s, (checks (r,s,i) ++ customAxioms i ++ axioms' ++ s.extra.constraints, maxims (r, s, i)))) results
    anyM (uncurry (smtcheck decls' memAxioms) . thd3) constraintss2 `orM`
        do
            let constraintss = map (\(r,s) -> (r, s, (customAxioms i ++ axioms' ++ s.extra.constraints, []))) results
            feasible <- filterM (uncurry (smtcheck decls' memAxioms) . thd3) constraintss
            results <- forM feasible (\(r, s, _) ->
                let state' = s{
                    caller = Var ("Caller" ++ show (i+1)),
                    callValue = Var ("CallValue" ++ show (i+1)),
                    callData = Var ("CallData" ++ show (i+1)),
                    pc = 0,
                    memory = mem 0,
                    extra=s.extra{callDataSize = Var ("CallDataSize" ++ show (i+1))} } in
                check_ (depth-1) state' decls' memAxioms axioms' checks maxims customAxioms (i+1))
            return $ or results

check :: Integer -> ((Expr, State, Integer) -> [Expr]) -> ((Expr, State, Integer) -> [Expr]) -> [Expr] -> [Expr] -> (Integer -> [Expr]) -> Program -> IO Bool
check depth checks maxims memAxioms baseAxioms customAxioms program =
    check_ depth (baseState program) baseDecls memAxioms baseAxioms checks maxims customAxioms 1

storeAxioms :: Expr -> [(Expr, Expr)] -> [Expr]
storeAxioms e = map (\(k,v) -> (e @! k) `eq` v)

oneOf :: Expr -> [Expr] -> Expr
oneOf e = foldr1 bvor . map (e `eq`)

buildAxioms :: [Expr] -> [(Expr, Expr)] -> [(Expr, Expr)] -> Integer -> [Expr]
buildAxioms attackers balances storage n =
    [ Var ("Caller" ++ show i) `oneOf` attackers | i <- [1..n]] ++
    storeAxioms (Var "Balances") balances ++
    storeAxioms (Var "Storage") storage
