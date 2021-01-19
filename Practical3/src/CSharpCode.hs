module CSharpCode where

import Prelude hiding (LT, GT, EQ)
import qualified Data.Map as M

import CSharpLex
import CSharpGram
import CSharpAlgebra
import SSM

import Data.Char (ord)


{-
  This file contains a starting point for the code generation which should handle very simple programs.
-}

--                           clas memb stat  expr
codeAlgebra :: CSharpAlgebra Code Code Code (ValueOrAddress -> Code)
codeAlgebra =
    ( fClas
    , (fMembDecl, fMembMeth)
    , (fStatDecl, fStatExpr, fStatIf, fStatWhile, fStatFor, fStatReturn, fStatCall, fStatBlock)
    , (fExprCon, fExprVar, fExprOp)
    )


fClas :: Token -> [Code] -> Code
fClas c ms = [Bsr "main", HALT] ++ concat ms

fMembDecl :: Decl -> Code
fMembDecl d = []

fMembMeth :: Type -> Token -> [Decl] -> Code -> Code
fMembMeth t (LowerId x) ps s = [LABEL x] ++ s ++ [RET]

fStatDecl :: Decl -> Code
fStatDecl d = []

fStatExpr :: (ValueOrAddress -> Code) -> Code
fStatExpr e = e Value ++ [pop]

fStatIf :: (ValueOrAddress -> Code) -> Code -> Code -> Code
fStatIf e s1 s2 = c ++ [BRF (n1 + 2)] ++ s1 ++ [BRA n2] ++ s2
    where
        c        = e Value
        (n1, n2) = (codeSize s1, codeSize s2)

fStatWhile :: (ValueOrAddress -> Code) -> Code -> Code
fStatWhile e s1 = [BRA n] ++ s1 ++ c ++ [BRT (-(n + k + 2))]
    where
        c = e Value
        (n, k) = (codeSize s1, codeSize c)

fStatFor :: [Code] -> (ValueOrAddress -> Code) -> [Code] -> Code -> Code
fStatFor ss1 e ss2 stats = concat ss1 ++ (fStatWhile e (stats ++ (concat ss2) ) )

fStatReturn :: (ValueOrAddress -> Code) -> Code
fStatReturn e = e Value ++ [pop] ++ [RET]

fStatCall :: Token -> [(ValueOrAddress -> Code)] -> Code
fStatCall (LowerId "print") es = concat (map (\x -> x Value) es) ++ [TRAP 0]
fStatCall (LowerId x) es = concat (map (\x -> x Value) es) ++ [Bsr x]

fStatBlock :: [Code] -> Code
fStatBlock = concat

fExprCon :: Token -> ValueOrAddress -> Code
fExprCon (ConstInt n) va = [LDC n]
fExprCon (TokenBool True) va = [LDC 1]
fExprCon (TokenBool False) va = [LDC 0]
fExprCon (TokenChar c) va = [LDC (ord c)]

fExprVar :: Token -> ValueOrAddress -> Code
fExprVar (LowerId x) va = let loc = 37 in case va of
                                              Value    ->  [LDL  loc]
                                              Address  ->  [LDLA loc]

fExprOp :: Token -> (ValueOrAddress -> Code) -> (ValueOrAddress -> Code) -> ValueOrAddress -> Code
fExprOp (Operator "=") e1 e2 va = e2 Value ++ [LDS 0] ++ e1 Address ++ [STA 0]
fExprOp (Operator "&&") e1 e2 va = e1 Value ++ e1 Value ++ [BRF (codeSize (e2 Value)+1)] ++ e2 Value ++ [AND] -- set e1 twice on stack: once for evaluation on branch, once as result or to calculate further with
fExprOp (Operator "||") e1 e2 va = e1 Value ++ e1 Value ++ [BRT (codeSize (e2 Value)+1)] ++ e2 Value ++ [OR]  -- set e1 twice on stack: once for evaluation on branch, once as result or to calculate further with
fExprOp (Operator op)  e1 e2 va = e1 Value ++ e2 Value ++ [opCodes M.! op]


opCodes :: M.Map String Instr
opCodes = M.fromList [ ("+", ADD), ("-", SUB),  ("*", MUL), ("/", DIV), ("%", MOD)
                   , ("<=", LE), (">=", GE),  ("<", LT),  (">", GT),  ("==", EQ)
                   , ("!=", NE), ("&&", AND), ("||", OR), ("^", XOR)
                   ]

-- | Whether we are interested in the value of a variable, or a pointer to it
data ValueOrAddress = Value | Address
    deriving Show
