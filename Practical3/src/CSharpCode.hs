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
codeAlgebra :: CSharpAlgebra Code Code Code Code (ValueOrAddress -> Code)
codeAlgebra =
    ( fClas
    , (fMembDecl, fMembMeth)
    , (fParameter)
    , (fStatDecl, fStatExpr, fStatIf, fStatWhile, fStatFor, fStatReturn, fStatCall, fStatBlock)
    , (fExprCon, fExprVar, fExprOp)
    )


fClas :: Env -> Token -> [Code] -> (Env, Code)
fClas env c ms = (env, [Bsr "main", HALT] ++ concat ms)

fMembDecl :: Env -> Decl -> (Env, Code)
fMembDecl env (Decl _ (LowerId x) ) = (M.insert x ((M.size env)+1) env, [])

fMembMeth :: Env -> Type -> Token -> [Decl] -> Code -> (Env, Code)
fMembMeth env t (LowerId x) ps s = (env, [LABEL x] ++ startup ++ s ++ cleanup ++ [RET])
    where startup = [LDR MP] ++ [LDRR MP SP] ++ [AJS lengthLocal]
          cleanup = [LDRR SP MP] ++ [STR MP] ++ [STS (-n)] ++ [AJS (-(n-1))]
          n = length ps
          lengthLocal = M.size (M.filter (> 0) env) 

fParameter :: Env -> Decl -> (Env, Code)
fParameter env(Decl _ (LowerId x) ) = case env == M.empty of 
                                          True -> (M.insert x (-2) env, [])
                                          False -> (M.insert x ((minimum env)-1) env, [])
    where f :: Int -> Int -> Int
          f x r = min x r
          minimum env = foldr f (-1) env

fStatDecl :: Env -> Decl -> (Env, Code)
fStatDecl env d = fMembDecl env d

fStatExpr :: Env -> (ValueOrAddress -> Code) -> (Env, Code)
fStatExpr env e = (env, e Value)                            -- deleted ++[pop]

fStatIf :: Env -> (ValueOrAddress -> Code) -> Code -> Code -> (Env, Code)
fStatIf env e s1 s2 = (env, c ++ [BRF (n1 + 2)] ++ s1 ++ [BRA n2] ++ s2)
    where
        c        = e Value
        (n1, n2) = (codeSize s1, codeSize s2)

fStatWhile :: Env -> (ValueOrAddress -> Code) -> Code -> (Env, Code)
fStatWhile env e s1 = (env, [BRA n] ++ s1 ++ c ++ [BRT (-(n + k + 2))])
    where
        c = e Value
        (n, k) = (codeSize s1, codeSize c)

fStatFor :: Env -> [Code] -> (ValueOrAddress -> Code) -> [Code] -> Code -> (Env, Code)
fStatFor env ss1 e ss2 stats = (env1, concat ss1 ++ code)
    where (env1, code) = (fStatWhile env e (stats ++ (concat ss2) ) ) 

fStatReturn :: Env -> (ValueOrAddress -> Code) -> (Env, Code)
fStatReturn env e = (env, e Value ++ [pop] ++ [RET])

fStatCall :: Env -> Token -> [(ValueOrAddress -> Code)] -> (Env, Code)
fStatCall env (LowerId "print") es = (env, concat (map (\x -> x Value) es) ++ [TRAP 0])
fStatCall env (LowerId x) es = (env, concat (map (\x -> x Value) es) ++ [Bsr x])

fStatBlock :: Env -> [Code] -> (Env, Code)
fStatBlock env c = (env, concat c)

fExprCon :: Env -> Token -> ValueOrAddress -> Code
fExprCon env (ConstInt n) va = [LDC n]
fExprCon env (TokenBool True) va = [LDC 1]
fExprCon env (TokenBool False) va = [LDC 0]
fExprCon env (TokenChar c) va = [LDC (ord c)]

fExprVar :: Env -> Token -> ValueOrAddress -> Code
fExprVar env (LowerId x) va = let loc = 37 in case va of
                                              Value    ->  case M.lookup x env of 
                                                            Just a -> [LDL a]
                                                            Nothing -> error ("Variable " ++ x ++ "not declared") 
                                              Address  ->  case M.lookup x env of 
                                                            Just loc -> [STL loc]
                                                            Nothing -> error ("Variable " ++ x ++ "not declared") 
                                                            

fExprOp :: Env -> Token -> (ValueOrAddress -> Code) -> (ValueOrAddress -> Code) -> ValueOrAddress -> Code
fExprOp env (Operator "=") e1 e2 va = e2 Value ++ e1 Address
fExprOp env (Operator "&&") e1 e2 va = e1 Value ++ e1 Value ++ [BRF (codeSize (e2 Value)+1)] ++ e2 Value ++ [AND] -- set e1 twice on stack: once for evaluation on branch, once as result or to calculate further with
fExprOp env (Operator "||") e1 e2 va = e1 Value ++ e1 Value ++ [BRT (codeSize (e2 Value)+1)] ++ e2 Value ++ [OR]  -- set e1 twice on stack: once for evaluation on branch, once as result or to calculate further with
fExprOp env (Operator op)  e1 e2 va = e1 Value ++ e2 Value ++ [opCodes M.! op]


opCodes :: M.Map String Instr
opCodes = M.fromList [ ("+", ADD), ("-", SUB),  ("*", MUL), ("/", DIV), ("%", MOD)
                   , ("<=", LE), (">=", GE),  ("<", LT),  (">", GT),  ("==", EQ)
                   , ("!=", NE), ("&&", AND), ("||", OR), ("^", XOR)
                   ]

-- | Whether we are interested in the value of a variable, or a pointer to it
data ValueOrAddress = Value | Address
    deriving Show
