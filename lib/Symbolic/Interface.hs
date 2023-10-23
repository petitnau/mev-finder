{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE RecordWildCards #-}

module Symbolic.Interface where

import Ast
import GenericSem
import Symbolic.Sem
import Symbolic.SMT

import Prelude hiding (id)
import Data.Maybe (mapMaybe, isJust, catMaybes, fromJust)
import Data.Tuple.Extra (thd3, both, dupe)
import Utils (anyM, orM, (<<), trimap, scanM, (<.>), iterateBoundedM, trimap', bimap', foldbi, foldtri, biliftA2)
import Control.Monad (forM, forM_, filterM)
import Control.Monad.Extra ((>=>), concatMapM)
import Data.Functor ((<&>))
import Data.Function ((&))
import Symbolic.MEV (hasMEV, extractedValue)
import qualified Concrete.MEV as CMEV
import Data.Tuple.HT (mapFst)
import Concrete.Interface (Call (Call), setStorage, setBalance, setAllowance, callId)
import Data.Bifunctor (Bifunctor(bimap, second))
import Concrete.Uint256 (touInteger, Uint256, touInt)
import qualified Concrete.Memory as Memory
import qualified Concrete.Sem as CS
import qualified Concrete.Interface as CI
import Debug.Trace (trace, traceShowId, traceWith)
import Crypto.Hash.Keccak (keccak256)
import qualified Data.ByteArray as BA
import Concrete.Memory (fromUint256)
import System.Log.Logger (infoM)
import Data.Foldable (foldrM)
import Test.Tasty.Runners (getTime)
import System.Random (getStdGen, Random (randomR), newStdGen)
import Env (domain, get)
import Concrete.Sem (prettyState)
import qualified Data.Map.Strict as M
import qualified Data.Set as S

data StorageKey
    = SPos Integer
    | SMap StorageKey Integer

(@@) :: StorageKey -> Integer -> StorageKey
(@@) = SMap

sktoexpr :: StorageKey -> Expr
sktoexpr (SPos l) = word l
sktoexpr (SMap p k) = BinOp Select (Var "hashfn") (LBitVec 512 0 +++ word k +++ sktoexpr p)

sktouint :: StorageKey -> Uint256
sktouint (SPos l) = fromInteger l
sktouint (SMap p k) = Memory.toUint256 . keccak256 $ Memory.fromSizedInteger 512 0 `BA.append` Memory.fromUint256 (fromInteger k) `BA.append` Memory.fromUint256 (sktouint p)

baseState :: Program -> State
baseState prog = State
    { id = Var "Id"
    , caller = Var "Caller0"
    , callValue = Var "CallValue0"
    , callData = Var "CallData0"
    , number = Var "BlockNumber"
    , timestamp = Var "Timestamp"
    , program = prog
    , pc = 0
    , balance = Balance { balances = Var "Balances", allowances = Var "Allowances"  }
    , stack = []
    , memory = mem 0
    , returned = mem 0
    , storage = Var "Storage"
    , extra = Extra {constraints = [], callDataSize = Var "CallDataSize0", returnDataSize = word 0, exprEnv = [], varcount = 0} }

baseAxioms :: [Expr]
baseAxioms =
        [ Var "Id" `eq` word 0 ]

baseDecls :: [Decl]
baseDecls =
    [ DeclVar "Id" tword
    , DeclVar "BlockNumber" tword
    , DeclVar "Timestamp" tword
    , DeclVar "Balances" (TArray (TBitVec 512) tword)
    , DeclVar "Allowances" (TArray (TBitVec 768) tword)
    , DeclVar "Storage" (TArray tword tword)
    ]

declsCall :: Integer -> [Decl]
declsCall i =
    [ DeclVar ("TxId" ++ show i) tword
    , DeclVar ("Caller" ++ show i) tword
    , DeclVar ("CallValue" ++ show i) tword
    , DeclVar ("CallDataSize" ++ show i) tword
    , DeclVar ("CallData" ++ show i) tmem ]

axiomsCall :: Integer -> [Expr]
axiomsCall i =
    [ ]
    -- Var ("CallData" ++ show i) `eq` (Var ("CallData" ++ show i) `bvand` ((mem 0 `bvsub` mem 1) `bvshl` (mem 4096 `bvsub` ((LBitVec 3840 0 +++ Var ("CallDataSize" ++ show i)) `bvmul` mem 8)))) ]

checkFromState :: Integer -> (Expr, State) -> [Decl] -> [Expr] -> [Expr] -> ((Expr, State, Integer) -> [Expr]) -> ((Expr, State, Integer) -> Expr) -> (Integer -> [Expr]) -> Integer -> IO Integer
checkFromState i (r,s) decls memAxioms axioms checks maxim customAxioms min = do
    let decls'  = decls  ++ declsCall i
    let axioms' = axioms ++ axiomsCall i
    let (c, m) = (eof s $ ManyOp Land (checks (r,s,i) ++ customAxioms i ++ axioms' ++ fixdatasizes s.extra.constraints), eof s $ maxim (r, s, i))
    maximize decls' memAxioms c m min

checkBFS :: Integer -> [State] -> [Decl] -> [Expr] -> [Expr] -> ((Expr, State, Integer) -> [Expr]) -> ((Expr, State, Integer) -> Expr) -> (Integer -> [Expr]) -> Integer -> Integer -> IO Integer
checkBFS maxDepth [] decls memAxioms axioms checks maxim customAxioms i min = return min
checkBFS maxDepth states decls memAxioms axioms checks maxim customAxioms i min = if maxDepth == i then return min else do
    t <- getTime
    infoM "" $ "@@ Starting depth " ++ show i ++ " at time " ++ show t
    let decls'  = decls  ++ declsCall i
    let axioms' = axioms ++ axiomsCall i
    executed <- concatMapM (sem (\c -> smtcheck decls' memAxioms (ManyOp Land $ fixdatasizes c ++ customAxioms i ++ axioms')) (\c -> c & normalize decls' & simplify decls')) states
    let returned = mapMaybe (\case Returned (r,s) -> Just (r,s); _ -> Nothing) executed
    infoM "" $ "Semantics executed, " ++ show (length returned) ++ " branch possible"
    results1 <- scanM (\min (r,s) -> checkFromState i (r,s) decls memAxioms axioms checks maxim customAxioms min) min returned
    let nextLayer = map (\(r,s) -> s{
            caller = Var ("Caller" ++ show (i+1)),
            callValue = Var ("CallValue" ++ show (i+1)),
            callData = Var ("CallData" ++ show (i+1)),
            pc = 0,
            memory = mem 0,
            stack = [],
            returned = mem 0,
            extra=s.extra{callDataSize = Var ("CallDataSize" ++ show (i+1))}}) returned
    checkBFS maxDepth nextLayer decls' memAxioms axioms' checks maxim customAxioms (i+1) (maximum $ min:results1)

check :: Integer -> ((Expr, State, Integer) -> [Expr]) -> ((Expr, State, Integer) -> Expr) -> [Expr] -> (Integer -> [Expr]) -> Program -> IO Integer
check depth checks maxim memAxioms customAxioms program = do
    checkBFS depth [baseState program] baseDecls memAxioms baseAxioms checks maxim customAxioms 0 0

concreteToSymbolic :: CS.CoreState -> [Expr]
concreteToSymbolic CS.CoreState{..} =
    let storagePairs = map (second (fromJust . Env.get coreStorage) . dupe) . Env.domain $ coreStorage in
    let storageExprs = map (\(k,v) -> (Var "Storage" @! (word . touInteger) k ) `eq` word (touInteger v)) storagePairs in
    let balancesPairs = map (second (fromJust . Env.get coreBalance) . dupe) . Env.domain $ coreBalance in
    let balancesExprs = map (\(k,v) -> (Var "Balances" @! (foldbi (+++) . bimap' (word . touInteger) $ k)) `eq` word (touInteger v)) balancesPairs in
    let allowancesPairs = map (second (fromJust . Env.get coreAllowances) . dupe) . Env.domain $ coreAllowances in
    let allowancesExprs = map (\(k,v) -> (Var "Allowances" @! (foldtri (+++) . trimap' (word . touInteger) $ k)) `eq` word (touInteger v)) allowancesPairs in
    storageExprs ++ balancesExprs ++ allowancesExprs

randomcheck :: ((Expr, State, Integer) -> [Expr]) -> [Expr] -> Program -> CS.State -> S.Set CS.CoreState -> M.Map CS.CoreState (S.Set (Integer, Integer, Integer, Integer), S.Set (Integer, Integer, Integer, Integer)) -> [Integer] ->  [(Integer, Integer)] -> Integer -> IO Integer
randomcheck checks memAxioms program firststate marked boundary attackers worth found = if null boundary then return found else do
    t <- getTime
    gen <- newStdGen
    let boundaryEntries = M.toList boundary
    let (i, _) = randomR (0, length boundaryEntries - 1) gen
    let (chosen, (takenedges, mempool)) = boundaryEntries !! i
    let decls'  = baseDecls  ++ declsCall 0
    let axioms' = baseAxioms ++ axiomsCall 0
    let stateaxioms = concreteToSymbolic chosen
    let state = baseState program
    let baseconstr = stateaxioms ++ axioms' ++ randomCallAxioms attackers (S.toList mempool)
    executed <- [state] & concatMapM (sem (\c -> smtcheck decls' memAxioms (ManyOp Land $ fixdatasizes c ++ baseconstr)) (\c -> c & normalize decls' & simplify decls')) <&> mapMaybe (\case Returned (r,s) -> Just s; _ -> Nothing)
    let newinputconstr = ManyOp Land $ LBool True:map (\(caller, callvalue, calldata, txid) -> UnOp LNot . ManyOp Land $
            [ BinOp Eq (Var "Caller0") (word caller)
            , BinOp Eq (Var "CallValue0") (word callvalue)
            , BinOp Eq (Var "CallData0") (mem calldata)
            , BinOp Eq (Var "TxId0") (word txid) ]) (S.toList takenedges)
    let fullconstr = ManyOp Land ((ManyOp Lor $ LBool False:(map (\s -> eof s $ ManyOp Land . fixdatasizes $ s.extra.constraints) executed)):newinputconstr:baseconstr)
    !input <- smtrandom decls' memAxioms fullconstr
    case input of
        Just input@(caller, callvalue, calldata, calldatasize, txid) -> do
            let initstate = CI.setCore chosen (CI.emptyState program)
            let endstate = CI.sequenceCalls initstate [(caller, callvalue, calldata, calldatasize)]
            let extracted = touInteger $ CMEV.extractedValue (map fromInteger attackers) (M.fromList $ map (bimap fromInteger fromInteger) worth) (firststate, endstate)
            t <- getTime
            if extracted > found then do
                infoM "" $ "!! Found EV of " ++ show extracted ++ " at time " ++ show t
            else return ()
            let endcore = (CI.getCore endstate)
            let newboundary = if endcore `S.member` marked then boundary else boundary & M.insertWith (biliftA2 S.union S.union) endcore (mempty, S.delete (caller, callvalue, calldata, txid) mempool)
            let newboundary' = newboundary & M.insertWith (biliftA2 S.union S.union) chosen (S.singleton (caller, callvalue, calldata, txid), mempty)
            randomcheck checks memAxioms program firststate marked newboundary' attackers worth (max extracted found)
        Nothing -> do
            let newboundary = boundary & M.delete chosen
            randomcheck checks memAxioms program firststate (S.insert chosen marked) newboundary attackers worth found

liftR :: (State -> a) -> ((Expr, State, Integer) -> Maybe a)
liftR f (_, s, _) = Just $ f s

memPoolAxioms :: [((Integer, Integer), Integer)] -> [((Integer, Integer, Integer), Integer)] -> [(StorageKey, Integer)] -> Program -> Call -> [Expr]
memPoolAxioms balances allowances storage p call =
    let state = CI.baseState p call in
    let state1 = foldr (setStorage . bimap sktouint fromInteger) state storage in
    let state2 = foldr (setBalance . bimap (both fromInteger) fromInteger) state1 balances in
    let state3 = foldr (setAllowance . bimap (trimap fromInteger fromInteger fromInteger) fromInteger) state2 allowances in
    CI.getConstraints (CS.sem state3)

checkMEV :: Integer -> [Integer] -> [Call] -> [(Integer, Integer)] -> [((Integer, Integer), Integer)] -> [((Integer, Integer, Integer), Integer)] -> [(StorageKey, Integer)] -> Program -> IO Integer
checkMEV depth attackers mempool worth balances allowances storage program =
    let attackers' = map word attackers in
    let worth' = map (bimap word word) worth in
    let !memAxioms =  concatMap (memPoolAxioms balances allowances storage program) mempool in
    check depth (\(_,s,_) -> [hasMEV attackers' worth' s]) (\(_,s,_) -> extractedValue attackers' worth' s) memAxioms (buildAxioms attackers' balances allowances storage mempool) program

checkMEV' :: Integer -> [Integer] -> [Call] -> [(Integer, Integer)] -> [((Integer, Integer), Integer)] -> [((Integer, Integer, Integer), Integer)] -> [(StorageKey, Integer)] -> Program -> IO Integer
checkMEV' depth attackers mempool worth balances allowances storage program =
    let state = CI.emptyState program in
    let state1 = foldr (setStorage . bimap sktouint fromInteger) state storage in
    let state2 = foldr (setBalance . bimap (both fromInteger) fromInteger) state1 balances in
    let state3 = foldr (setAllowance . bimap (trimap fromInteger fromInteger fromInteger) fromInteger) state2 allowances in
    let attackers' = map word attackers in
    let worth' = map (bimap word word) worth in
    let mempool' = S.fromList $ callsToIntegers mempool in
    let !memAxioms =  concatMap (memPoolAxioms balances allowances storage program) mempool in
    randomcheck (\(_,s,_) -> [hasMEV attackers' worth' s]) memAxioms program state3 mempty (M.singleton (CI.getCore state3) (mempty, mempool')) attackers worth 0

storeAxioms :: Expr -> [(Expr, Expr)] -> [Expr]
storeAxioms e = map (\(k,v) -> (e @! k) `eq` v)

oneOf :: Expr -> [Expr] -> Expr
oneOf e = foldr1 lor . map (e `eq`)

isCall :: Integer -> (Integer, Call) -> Expr
isCall i (txid, c@(Call caller fn params cv)) =
    foldr1 land
        [ Var ("Caller" ++ show i) `eq` (word . touInteger $ caller)
        , Var ("CallValue" ++ show i) `eq` (word . touInteger $ cv)
        , Var ("CallData" ++ show i) `eq` (mem. Memory.toInteger . Memory.padr 512  $ callId c)
        , Var ("TxId" ++ show i) `eq` word txid
        ]

callsToIntegers :: [Call] -> [(Integer, Integer, Integer, Integer)]
callsToIntegers = zipWith (\i c@(Call caller fn params cv) ->
            ( touInteger caller
            , touInteger cv
            , Memory.toInteger . Memory.padr 512 $ callId c
            , i )) [0..]

randomCallAxioms :: [Integer] -> [(Integer, Integer, Integer, Integer)] -> [Expr]
randomCallAxioms attackers pool =
    let attackercase = Var "Caller0" `oneOf` map word attackers in
    let poolcase = map (\(caller, callvalue, calldata, txid) -> ManyOp Land
            [ Var "Caller0" `eq` word caller
            , Var "CallValue0" `eq` word callvalue
            , Var "CallData0" `eq` mem calldata
            , Var "TxId0" `eq` word txid
            ]) pool in
    [ManyOp Lor (attackercase:poolcase)]

buildAxioms :: [Expr] -> [((Integer, Integer), Integer)] -> [((Integer, Integer, Integer), Integer)] -> [(StorageKey, Integer)] -> [Call] -> Integer -> [Expr]
buildAxioms attackers balances allowances storage mempool n =
    (ManyOp Distinct [Var ("TxId" ++ show i) | i <- [0..n]]) :
    [ foldr1 lor ((Var ("Caller" ++ show i) `oneOf` attackers):map (isCall i) (zip [0..] mempool)) | i <- [0..n]] ++
    storeAxioms (Var "Balances")   (map (\((a,b),x)   -> (word a+++word b,           word x)) balances) ++
    storeAxioms (Var "Allowances") (map (\((a,b,c),x) -> (word a+++word b+++word c, word x)) allowances) ++
    storeAxioms (Var "Storage") (map (bimap sktoexpr word) storage)
