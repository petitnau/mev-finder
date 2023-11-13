{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE BangPatterns #-}

module Symbolic.SMT where

import System.Process ( readProcessWithExitCode )
import Data.List
import Numeric
import Utils
import Data.Function ((&))
import Data.Functor ((<&>))
import Debug.Trace
import System.Directory (doesFileExist)

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
tmem = TBitVec 1024

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
    | Lt | Gt | SLt | SGt | Eq
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
        Xor -> "bvxor"
        Implies -> "=>"
        Select -> "select"
        Concat -> "concat"

data TerOp
    = AddMod | MulMod
    | Store | Extract | Ite
    deriving (Eq, Ord)

instance Show TerOp where
    show = \case
        Store -> "store"
        AddMod -> "AddMod"
        MulMod -> "MulMod"
        Extract -> "extract"
        Ite -> "ite"

data Expr
    = Var String
    | LInt Integer
    | LBitVec Int Integer
    | UnOp  UnOp  Expr
    | BinOp BinOp Expr Expr
    | TerOp TerOp Expr Expr Expr
    | ForAll String DeclType Expr
    deriving (Eq, Ord)

bv2nat, bvnot, lnot :: Expr -> Expr
bv2nat = UnOp BV2Nat
bvnot  = UnOp Not
lnot   = UnOp LNot

bvadd, bvmul, bvsub, bvudiv, bvsdiv, bvurem, bvsrem, bvshl, bvlshr, bvashr, bvult, bvugt, bvslt, bvsgt, eq, bvand, bvor, bvxor, (@!), (+++), implies :: Expr -> Expr -> Expr
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
bvand  = BinOp And
bvor   = BinOp Or
bvxor  = BinOp Xor
(@!)   = BinOp Select
(+++)  = BinOp Concat
implies = BinOp Implies

(@+) :: Expr -> (Expr, Expr) -> Expr
(@+) a (k, v) = TerOp Store a k v

extract :: Expr -> Expr -> Expr -> Expr
extract from length = TerOp Extract (bv2nat (word 1024 `bvsub` (word 1 `bvadd` from))) (bv2nat (word 1024 `bvsub` (length `bvadd` from)))

replace :: Expr -> Expr -> Expr -> Expr -> Expr
replace from new length bv =
    extract (word 0) from bv +++ new +++ extract (from `bvadd` length) (word 1024 `bvsub` (from `bvadd` length)) bv

bool2bitvec :: Expr -> Expr
bool2bitvec e = TerOp Ite e (word 1) (word 0)

makeFun :: [String] -> String
makeFun ss = "(" ++ unwords ss ++ ")"

instance Show Expr where
    show = \case
        Var s                   -> s
        LInt n                  -> show n
        LBitVec s n             -> "#b" ++ padBinary s (showBin n "")
        UnOp  op e1             -> makeFun [show op, show e1]
        BinOp op e1 e2          -> makeFun [show op, show e1, show e2]
        TerOp Extract e1 e2 e3  -> makeFun [makeFun ["_", show Extract, show e1, show e2], show e3]
        TerOp op e1 e2 e3       -> makeFun [show op, show e1, show e2, show e3]
        ForAll v t b            -> makeFun ["forall", makeFun [makeFun [v, show t]], show b]
word, mem, bit :: Integer -> Expr
word = LBitVec 256
mem  = LBitVec 1024
bit  = LBitVec 1

simplify :: Expr -> Expr
simplify (LBitVec s n) = LBitVec s n
simplify (UnOp BV2Nat e) = e & simplify & (\(LBitVec 256 n) ->  LInt n)
simplify (BinOp Add e1 e2) =
    case (simplify e1, simplify e2) of
        (LBitVec 256 a, LBitVec 256 b) -> LBitVec 256 (a+b)
simplify (BinOp Sub e1 e2) =
    case (simplify e1, simplify e2) of
        (LBitVec 256 a, LBitVec 256 b) -> LBitVec 256 (a-b)
simplify (BinOp Mul e1 e2) =
    case (simplify e1, simplify e2) of
        (LBitVec 256 a, LBitVec 256 b) -> LBitVec 256 (a*b)
simplify x = error $ show x


{-
simplify :: [Decl] -> Expr -> IO Expr
simplify ds e = do
    let smt = unlines (map show ds ++ ["(simplify " ++ show e ++ ")"])
    writeFile "simplify.smt" smt
    x <- readProcessWithExitCode "z3" ["simplify.smt"] ""
    let (code, stdout, stderr) = x
    return (LInt (read stdout :: Integer))
-}

isZeroExtract :: Expr -> Bool
isZeroExtract (TerOp Extract (LInt a) (LInt b) _)
    | a+1 == b  = True
    | otherwise = False
isZeroExtract _ = False

normalize :: [Decl] -> Expr -> IO Expr
normalize ds (Var s) = return $ Var s
normalize ds (LInt n) = return $ LInt n
normalize ds (LBitVec s n) = return $ LBitVec s n
normalize ds (UnOp Hash e1) = do
    ne1 <- normalize ds e1
    return $ case ne1 of
        TerOp Extract (LInt to) (LInt from) _ ->
            UnOp Hash $ (LBitVec (fromInteger $ 1024-(to-from+1)) 0 +++ ne1)
        _ -> UnOp Hash $ ne1
normalize ds (UnOp op e1) = do
    UnOp op <$> normalize ds e1
normalize ds (BinOp Concat e1 e2) = do
    ne1 <- normalize ds e1
    ne2 <- normalize ds e2
    return $
        if isZeroExtract ne1 then ne2
        else if isZeroExtract ne2 then ne1
        else BinOp Concat ne1 ne2
normalize ds (BinOp op e1 e2) = do
    BinOp op <$> normalize ds e1 <*> normalize ds e2
normalize ds (TerOp Extract e1 e2 e3) = do
    TerOp Extract <$> (e1 & normalize ds <&> simplify) <*> (e2 & normalize ds <&> simplify) <*> normalize ds e3
normalize ds (TerOp op e1 e2 e3) = do
    TerOp op <$> normalize ds e1 <*> normalize ds e2 <*> normalize ds e3
normalize ds (ForAll v t e) =
    ForAll v t <$> normalize ds e

smtcheck :: [Decl] -> [Expr] -> [Expr] -> [Expr] -> IO Bool
smtcheck ds hashConds conds maxims = do
    normconds <- mapM (normalize ds) conds
    normmaxims <- mapM (normalize ds) maxims
    let smt = unlines (
            map show ds ++
            ["(assert (forall((hashfn (Array (_ BitVec 1024) (_ BitVec 256))))"] ++ 
            ["\t(=>"] ++
            ["\t\t(and"] ++
            map (\hcond -> "\t\t\t" ++ show hcond ) hashConds ++
            ["\t\t)"] ++
            ["\t\t(and"] ++
            map (\cond -> "\t\t\t" ++ show cond ) normconds ++
            ["\t\t)))"] ++
            [")"] ++
            map (\expr -> "(maximize " ++ show expr ++ ")") normmaxims ++
            ["(check-sat)"] ++
            -- ["(get-model)"] ++
            map (\expr -> "(eval " ++ show expr ++ ")") normmaxims
            )
    --writeFile (if doesFileExist "check.smt" then "check1.smt" else "check.smt") smt
    writeFile "check.smt" smt

    x <- readProcessWithExitCode "z3" ["check.smt"] ""
    let (code, stdout, stderr) = x
    putStrLn stdout
    return $ not ("unsat" `isInfixOf` stdout)
