{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}

module Symbolic.SMT where

import Benchmark
import System.Process ( readProcessWithExitCode )
import Data.List
import Numeric
import Utils
import Data.Function ((&))
import Data.Functor ((<&>))
import Data.Composition
import Data.Maybe
import Data.Either.Extra (fromRight', fromLeft')
import qualified Data.ByteArray as BA
import qualified Data.ByteString as BS
import qualified Data.Binary as B
import qualified Data.BitVector as BV
import Debug.Trace
import qualified Concrete.Memory as Memory
import Data.List.Extra (trim, lower, groupOnKey)
import Data.Foldable (foldrM)
import Control.Monad (when)
import Control.Monad.Extra (whenM)
import Test.Tasty.Runners (getTime)
import System.Log.Logger (infoM)
import System.Random (getStdGen, Random (randomR))
import Data.Char (isDigit)
import Data.List.Key (group)

data DeclType
    = TInt
    | TBitVec Integer
    | TArray DeclType DeclType
    deriving (Eq, Ord)

instance Show DeclType where
    show = \case
        TInt -> "Int"
        TBitVec n -> "(_ BitVec " ++ show n ++ ")"
        TArray t1 t2 -> "(Array " ++ show t1 ++ " " ++ show t2 ++ ")"

tword :: DeclType
tword = TBitVec 256

tmem :: DeclType
tmem = TBitVec 4096

data Decl
    = DeclVar String DeclType
    | DeclFun String DeclType DeclType

instance Show Decl where
    show = \case
        DeclVar s t -> "(declare-const " ++ s ++ " " ++ show t ++ ")"
        DeclFun s t1 t2 -> "(declare-fun " ++ s ++ " (" ++ show t1 ++ ") " ++ show t2 ++ ")"

data UnOp
    = BV2Nat | SignExtend | Hash | Not | LNot
    deriving (Eq, Ord)

instance Show UnOp where
    show = \case
        BV2Nat -> "bv2nat"
        Not -> "bvnot"
        LNot -> "not"
        Hash -> "hashfn"

data BinOp
    = Add | Mul | Sub | Div | SDiv | Mod | SMod | Exp | Byte | Shl | Shr | Sar
    | Lt | Gt | SLt | SGt | Eq | Lor | Land | Distinct
    | And | Or | Xor | Implies
    | Select | Concat
    deriving (Eq, Ord)

instance Show BinOp where
    show = \case
        Add -> "bvadd"
        Mul -> "bvmul"
        Sub -> "bvsub"
        Div -> "bvudiv"
        SDiv -> "bvsdiv"
        Mod -> "bvurem"
        SMod -> "bvsrem"
        Exp -> "^"
        Byte -> "Byte"
        Shl -> "bvshl"
        Shr -> "bvlshr"
        Sar -> "bvashr"
        Lt -> "bvult"
        Gt -> "bvugt"
        SLt -> "bvslt"
        SGt -> "bvsgt"
        Eq -> "="
        And -> "bvand"
        Or -> "bvor"
        Lor -> "or"
        Land -> "and"
        Distinct -> "distinct"
        Xor -> "bvxor"
        Implies -> "=>"
        Select -> "select"
        Concat -> "concat"

data TerOp
    = AddMod | MulMod
    | Store | Ite
    deriving (Eq, Ord)

instance Show TerOp where
    show = \case
        Store -> "store"
        AddMod -> "AddMod"
        MulMod -> "MulMod"
        Ite -> "ite"

data Expr
    = Var String
    | LInt Integer
    | LBitVec Int Integer
    | LBool Bool
    | UnOp  UnOp  Expr
    | BinOp BinOp Expr Expr
    | TerOp TerOp Expr Expr Expr
    | ManyOp BinOp [Expr]
    | Extract Integer Integer Expr
    | ForAll String DeclType Expr
    | Let String Expr Expr
    deriving (Eq, Ord)

bv2nat, bvnot, lnot :: Expr -> Expr
bv2nat = UnOp BV2Nat
bvnot  = UnOp Not
lnot   = UnOp LNot

bvadd, bvmul, bvsub, bvudiv, bvsdiv, bvurem, bvsrem, bvshl, bvlshr, bvashr, bvult, bvugt, bvslt, bvsgt, eq, lor, land, bvand, bvor, bvxor, (@!), (+++), implies :: Expr -> Expr -> Expr
bvadd  = BinOp Add
bvmul  = BinOp Mul
bvsub  = BinOp Sub
bvudiv = BinOp Div
bvsdiv = BinOp SDiv
bvurem = BinOp Mod
bvsrem = BinOp SMod
bvshl  = BinOp Shl
bvlshr = BinOp Shr
bvashr = BinOp Sar
bvult  = BinOp Lt
bvugt  = BinOp Gt
bvslt  = BinOp SLt
bvsgt  = BinOp SGt
eq     = BinOp Eq
lor    = BinOp Lor
land   = BinOp Land
bvand  = BinOp And
bvor   = BinOp Or
bvxor  = BinOp Xor
(@!)   = BinOp Select
(+++)  = BinOp Concat
implies = BinOp Implies

(@+) :: Expr -> (Expr, Expr) -> Expr
(@+) a (k, v) = TerOp Store a k v

extract :: Integer -> Integer -> Integer -> Expr ->  Expr
extract fullsize from length =
    Extract (fullsize - 1 - from) (fullsize - length - from)

wextract, mextract :: Integer -> Integer -> Expr -> Expr
wextract = extract 256
mextract = extract 4096

replace :: Integer -> Integer -> Expr -> Integer -> Expr -> Expr
replace fullsize from new length bv =
    extract fullsize 0 from bv +++ new +++ extract fullsize (from + length) (fullsize - from - length) bv

wreplace, mreplace :: Integer -> Expr -> Integer -> Expr -> Expr
wreplace = replace 256
mreplace = replace 4096

bool2bitvec :: Expr -> Expr
bool2bitvec e = TerOp Ite e (word 1) (word 0)

makeFun :: [String] -> String
makeFun ss = "(" ++ unwords ss ++ ")"

instance Show Expr where
    show = \case
        Var s                   -> s
        LInt n                  -> show n
        -- LBitVec s n             -> "#b" ++ padBinary s (showBin n "")
        LBitVec s n             -> makeFun [makeFun ["_", "int2bv", show s], show n]
        LBool b                 -> lower $ show b
        UnOp  op e1             -> makeFun [show op, show e1]
        BinOp op e1 e2          -> makeFun [show op, show e1, show e2]
        TerOp op e1 e2 e3       -> makeFun [show op, show e1, show e2, show e3]
        Extract e1 e2 e3        -> makeFun [makeFun ["_", "extract", show e1, show e2], show e3]
        ForAll v t b            -> makeFun ["forall", makeFun [makeFun [v, show t]], show b]
        Let v t e               -> makeFun ["let", makeFun [makeFun [v, show t]], show e]
        ManyOp op es            -> makeFun (show op : map show es)

word, mem, bit :: Integer -> Expr
word = LBitVec 256
mem  = LBitVec 4096
bit  = LBitVec 1

concretize :: Expr -> Maybe (Either Integer BV.BitVector)
concretize (LBitVec s n) = return . Right $ BV.bitVec s n
concretize (LInt n) = return . Left $ n
concretize (UnOp BV2Nat e1) = do
    v1 <- concretize e1
    return . Left $ BV.uint (fromRight' v1)
concretize (BinOp And e1 e2) = do
    v1 <- concretize e1
    v2 <- concretize e2
    return . Right $ fromRight' v1 BV..&. fromRight' v2
concretize (BinOp Add e1 e2) = do
    v1 <- concretize e1
    v2 <- concretize e2
    return . Right $ fromRight' v1 + fromRight' v2
concretize (BinOp Sub e1 e2) = do
    v1 <- concretize e1
    v2 <- concretize e2
    return . Right $ fromRight' v1 - fromRight' v2
concretize (BinOp Mul e1 e2) = do
    v1 <- concretize e1
    v2 <- concretize e2
    return . Right $ fromRight' v1 * fromRight' v2
concretize (BinOp Shl e1 e2) = do
    v1 <- concretize e1
    v2 <- concretize e2
    return . Right $ fromRight' v1 `BV.shl` fromRight' v2
concretize (BinOp Concat e1 e2) = do
    v1 <- concretize e1
    v2 <- concretize e2
    return . Right $ fromRight' v1 BV.# fromRight' v2
concretize (ManyOp op el) = concretize $ foldr1 (BinOp op) el
concretize (Extract e1 e2 e3) = do
    v3 <- concretize e3
    return . Right $ BV.extract e1 e2 (fromRight' v3)
concretize x = Nothing

simplify1 :: Expr -> Maybe Expr
simplify1 e = do
    v <- concretize e
    --traceM (show (v,e))
    return $ case v of
        Left n -> LInt n;
        Right bv -> LBitVec (BV.size bv) (BV.uint bv)

parseZ3Num :: String -> Integer
parseZ3Num s
  | "#x" `isPrefixOf` s = fst . head . readHex . drop 2 $ s
  | all isDigit s = fst . head . readDec $ s
  | otherwise = 0

simplify :: [Decl] -> Expr -> IO Expr
simplify ds e = case simplify1 e of
    Just r -> return r
    Nothing -> do
        let smt = unlines (map show ds ++ ["(simplify " ++ show e ++ ")"])
        writeFile "simplify.smt" smt
        x <- readProcessWithExitCode "z3" ["simplify.smt"] ""
        let (code, stdout, stderr) = x
        if "#x" `isPrefixOf` stdout then do
            return (LBitVec ((length (trim stdout)-2) * 4) (fst . head $ readHex (tail . tail $ stdout)))
        else
            return (LInt (fst . head $ readDec stdout))

isZeroExtract :: Expr -> Bool
isZeroExtract (Extract a b _)
    | a+1 == b  = True
    | otherwise = False
isZeroExtract _ = False

finddatasizes :: [Expr] -> [(Integer, Integer)]
finddatasizes [] = []
-- (= (ite (bvult CallDataSize0 ((_ int2bv 256) 4)) ((_ int2bv 256) 1) ((_ int2bv 256) 0)) ((_ int2bv 256) 0))
finddatasizes ((BinOp Eq (TerOp Ite (BinOp Lt (Var cds) (LBitVec 256 4)) (LBitVec 256 1) (LBitVec 256 0)) (LBitVec 256 0)):r) =
    if "CallDataSize" `isPrefixOf` cds then (read . drop 12 $ cds, 4):finddatasizes r
    else finddatasizes r
finddatasizes ((UnOp LNot (BinOp Eq (TerOp Ite (BinOp Lt (Var cds) (LBitVec 256 4)) (LBitVec 256 1) (LBitVec 256 0)) (LBitVec 256 0))):r) =
    if "CallDataSize" `isPrefixOf` cds then (read . drop 12 $ cds, 4):finddatasizes r
    else finddatasizes r
-- (not (= (ite (= ((_ int2bv 256) 0) (ite (bvslt (bvsub (bvadd ((_ int2bv 256) 4) (bvsub CallDataSize0 ((_ int2bv 256) 4))) ((_ int2bv 256) 4)) ((_ int2bv 256) 32)) ((_ int2bv 256) 1) ((_ int2bv 256) 0))) ((_ int2bv 256) 1) ((_ int2bv 256) 0)) ((_ int2bv 256) 0))) 
finddatasizes ((UnOp LNot (BinOp Eq (TerOp Ite (BinOp Eq (LBitVec 256 0) (TerOp Ite (BinOp SLt (BinOp Sub (BinOp Add (LBitVec 256 4) (BinOp Sub (Var cds) (LBitVec 256 4))) (LBitVec 256 4)) (LBitVec 256 siz)) (LBitVec 256 1) (LBitVec 256 0))) (LBitVec 256 1) (LBitVec 256 0)) (LBitVec 256 0))):r) =
    if "CallDataSize" `isPrefixOf` cds then (read . drop 12 $ cds, 4+siz):finddatasizes r
    else finddatasizes r
finddatasizes ((BinOp Eq (TerOp Ite (BinOp Eq (LBitVec 256 0) (TerOp Ite (BinOp SLt (BinOp Sub (BinOp Add (LBitVec 256 4) (BinOp Sub (Var cds) (LBitVec 256 4))) (LBitVec 256 4)) (LBitVec 256 siz)) (LBitVec 256 1) (LBitVec 256 0))) (LBitVec 256 1) (LBitVec 256 0)) (LBitVec 256 0)):r) =
    if "CallDataSize" `isPrefixOf` cds then (read . drop 12 $ cds, 4+siz):finddatasizes r
    else finddatasizes r
finddatasizes (_:r) = finddatasizes r

fixdatasizes :: [Expr] -> [Expr]
fixdatasizes exprs =
    let sizes = finddatasizes exprs in
    let groups = groupOnKey fst sizes in
    let maxs = map (\(k, v) -> (k, maximum $ map snd v)) groups in
    let newconstrs = map (\(k, max) -> BinOp Eq (Var ("CallDataSize" ++ show k)) (LBitVec 256 max)) maxs in
    let newconstrs2 = map (\(k, _) -> Var ("CallData" ++ show k) `eq` (Var ("CallData" ++ show k) `bvand` ((mem 0 `bvsub` mem 1) `bvshl` (mem 4096 `bvsub` ((LBitVec 3840 0 +++ Var ("CallDataSize" ++ show k)) `bvmul` mem 8))))) maxs in
    newconstrs2 ++ newconstrs ++ exprs

normalize :: [Decl] -> Expr -> Expr
normalize ds (Var s) = Var s
normalize ds (LInt n) = LInt n
normalize ds (LBool b) = LBool b
normalize ds (LBitVec s n) = LBitVec s n
normalize ds (BinOp Select (Var "hashfn") e1) = do
    let ne1 = normalize ds e1
    case ne1 of
        Extract to from _ ->
            BinOp Select (Var "hashfn") (LBitVec (fromInteger $ 1024-(to-from+1)) 0 +++ ne1)
        _ -> BinOp Select (Var "hashfn") ne1
normalize ds (UnOp op e1) = do
    UnOp op (normalize ds e1)
normalize ds (BinOp Concat e1 e2) = do
    let ne1 = normalize ds e1
    let ne2 = normalize ds e2
    if isZeroExtract ne1 then ne2
    else if isZeroExtract ne2 then ne1
    else BinOp Concat ne1 ne2
normalize ds (BinOp op e1 e2) = do
    BinOp op (normalize ds e1) (normalize ds e2)
normalize ds (ManyOp op el) = do
    ManyOp op (map (normalize ds) el)
normalize ds (Extract e1 e2 e3) = do
    Extract e1 e2 (normalize ds e3)
normalize ds (TerOp op e1 e2 e3) = do
    TerOp op (normalize ds e1) (normalize ds e2) (normalize ds e3)
normalize ds (ForAll v t e) =
    ForAll v t (normalize ds e)
normalize ds (Let v t e) =
    Let v (normalize ds t) (normalize ds e)

timeout :: Integer
timeout = 600*s
    where s = 1000

maximize :: [Decl] -> [Expr] -> Expr -> Expr -> Integer -> IO Integer
maximize ds hashConds cond maxim min = do
    let normcond = normalize ds cond
    let normhconds = map (normalize ds) hashConds
    let normmaxim = normalize ds maxim
    let checked = BinOp Land normcond (normmaxim `bvugt` word min)
    let (hashed, asserted) =
            if "hashfn" `isInfixOf` show normcond then
                (True, ForAll "hashfn" (TArray (TBitVec 1024) (TBitVec 256)) (ManyOp Land (LBool True:normhconds) `implies` checked))
            else
                (False, checked)
    let smt = unlines (
            ["(set-option :timeout "++ show timeout ++ ")"] ++
            map show ds ++
            ["(assert " ++ show asserted ++ ")"] ++
            (if not hashed then [] else
                [ "(set-simplifier (then simplify solve-eqs elim-unconstrained propagate-values simplify qe-light simplify qe-light))"
                , "(apply (then simplify))" ]) ++
            ["(check-sat)"] ++
            ["(eval " ++ show maxim ++ ")"])
    z <- writeFile "check1.smt" smt
    x <- readProcessWithExitCode "z3" ["check1.smt"] ""
    let (code, stdout, stderr) = x
    let [eval, result] = map trim . take 2 . reverse $ lines stdout
    if result == "sat" then do
        let eval1 = parseZ3Num eval
        t <- getTime
        infoM "" $ "!! Found EV of " ++ show eval1 ++ " at time " ++ show t
        evalm <- maximize ds hashConds cond maxim eval1
        return $ max evalm min
    else
        return min

smtcheck :: [Decl] -> [Expr] -> Expr -> IO Bool
smtcheck ds hashConds cond = do
    let normcond = normalize ds cond
    let normhconds = map (normalize ds) hashConds
    let (hashed, asserted) =
            if "hashfn" `isInfixOf` show normcond then
                (True, ForAll "hashfn"  (TArray (TBitVec 1024) (TBitVec 256)) (ManyOp Land normhconds `implies` normcond))
            else
                (False, normcond)
    let smt = unlines (
            ["(set-option :timeout "++ show timeout ++")"] ++
            map show ds ++
            ["(assert " ++ show asserted ++ ")"] ++
            (if not hashed then [] else
                [ "(set-simplifier (then simplify solve-eqs elim-unconstrained propagate-values simplify qe-light simplify qe-light))"
                , "(apply (then simplify))" ]) ++
            ["(check-sat)"])
    writeFile "check.smt" smt
    x <- readProcessWithExitCode "z3" ["check.smt"] ""
    let (code, stdout, stderr) = x
    let result = trim . last $ lines stdout
    return (result == "sat")


smtrandom :: [Decl] -> [Expr] -> Expr -> IO (Maybe (Integer, Integer, Integer, Integer, Integer))
smtrandom ds hashConds cond = do
    let normcond = normalize ds cond
    let normhconds = map (normalize ds) hashConds
    let (hashed, asserted) =
            if "hashfn" `isInfixOf` show normcond then
                (True, ForAll "hashfn"  (TArray (TBitVec 1024) (TBitVec 256)) (ManyOp Land (LBool True : normhconds) `implies` normcond))
            else
                (False, normcond)
    let evals = [ Var "Caller0", Var "CallValue0", Var "CallData0", Var "CallDataSize0", Var "TxId0" ]
    let smt = unlines (
            ["(set-option :timeout "++ show timeout ++")"] ++
            map show ds ++
            ["(assert " ++ show asserted ++ ")"] ++
            (if not hashed then [] else
                [ "(set-simplifier (then simplify solve-eqs elim-unconstrained propagate-values simplify qe-light simplify qe-light))"
                , "(apply (then simplify))" ]) ++
            ["(check-sat)"] ++
            map (\e -> "(eval " ++ show e ++ ")") evals)
    writeFile "random.smt" smt
    x <- readProcessWithExitCode "z3" ["random.smt", "-T:"++ show timeout] ""
    let (code, stdout, stderr) = x
    let result = trim . (!! 5) . reverse $ lines stdout
    if result == "sat" then
        return .  Just $ (\[a,b,c,d,e] -> (a,b,c,d,e)) . map (parseZ3Num . trim) . reverse . take 5 . reverse $ lines stdout
    else
        return  Nothing
