module CSharpAlgebra where

import CSharpLex
import CSharpGram
import qualified Data.Map as M

{-
  Only modify this file when you change the AST in CSharpGram.hs
-}

type CSharpAlgebra clas memb par stat expr                              --
    =    ( Env -> Token -> [memb] -> (Env, clas)                    --  Class = Class Token [Member]
                                                                    --
      ,  ( Env -> Decl                             -> (Env, memb)   --  Member = MemberD Decl
         , Env -> Type -> Token -> [Decl] -> stat  -> (Env, memb)   --         | MemberM Type Token [Decl] Stat
         )                                                          --
                                                                    --
      ,  ( Env -> Decl -> (Env, par))                              -- Parameter

      ,  ( Env -> Decl                  -> (Env, stat)              --  Stat = StatDecl   Decl
         , Env -> expr                  -> (Env, stat)              --       | StatExpr   Expr
         , Env -> expr -> stat -> stat  -> (Env, stat)              --       | StatIf     Expr Stat Stat
         , Env -> expr -> stat          -> (Env, stat)              --       | StatWhile  Expr Stat
         , Env -> [stat]->expr->[stat] -> stat -> (Env, stat)       --       | StatFor    [Stat] Expr [Stat] Stat
         , Env -> expr                  -> (Env, stat)              --       | StatReturn Expr
         , Env -> Token -> [expr]       -> (Env, stat)              --       | StatCall Expr
         , Env -> [stat]                -> (Env, stat)              --       | StatBlock  [Stat]
         )                                                          --
                                                                    --
      ,  ( Env -> Token                  -> expr                    --  Expr = ExprConst  Token
         , Env -> Token                  -> expr                    --       | ExprVar    Token
         , Env -> Token -> expr -> expr  -> expr                    --       | ExprOper   Token Expr Expr
         )                                                          --
      )


foldCSharp :: CSharpAlgebra clas memb par stat expr -> Class -> (Env, clas)
foldCSharp (c, (md,mm), (pp), (sd,se,si,sw,sf,sr,sc,sb), (ec,ev,eo)) cl = fClas ev_ cl
    where
        fClas env (Class      t ms)     = let (env1, code) = updateMapEnv fMemb env ms in c env1 t code --(map (fMemb env) ms)
        fMemb env (MemberD    d)        = md env d
        fMemb env (MemberM    t m ps s) = let (env_p, c) = updateMapEnv fPar ev_ ps in let (env1, code) = (fStat env_p s) in mm env1 t m ps code --to do let (env2, code2) = mm env1 t m ps code in (env, code2) --(fStat env s)
        fPar  env ps                    = pp env ps
        fStat env (StatDecl   d)        = sd env d
        fStat env (StatExpr   e)        = se env (fExpr env e)
        fStat env (StatIf     e s1 s2)  = let (env2, code2) = (fStat env s1) in
                                             let (env3, code3) = (fStat env2 s2) in
                                                 si env3 (fExpr env e) code2 code3 --(fExpr env e) (fStat env s1) (fStat env s2)
        fStat env (StatWhile  e s1)     = let (env1, statcode) = (fStat env s1) in sw env1 (fExpr env e) statcode
        fStat env (StatFor ss1 e ss2 s) = let (env1, code1) = (updateMapEnv fStat env ss1) in 
                                              let code2 = (fExpr env1 e) in
                                                 let (env3, code3) = (updateMapEnv fStat env1 ss2) in
                                                    let (env4, code4) = (fStat env3 s) in                                                 
                                                 sf env4 code1 code2 code3 code4 -- (map (fStat env) ss1) (fExpr env e) (map (fStat env) ss2) (fStat env s)

        fStat env (StatReturn e)        = sr env (fExpr env e)
        fStat env (StatCall t es)       = sc env t (map (fExpr env) es)
        fStat env (StatBlock  ss)       = let (env1, code) = updateMapEnv fStat env ss in sb env1 code --(map (fStat env) ss)
        fExpr env (ExprConst  con)      = ec env con
        fExpr env (ExprVar    var)      = ev env var
        fExpr env (ExprOper   op e1 e2) = eo env op (fExpr env e1) (fExpr env e2)
        updateMapEnv func env xs = foldr (\x (e,c) -> let (e1, c1) = (func e x) in (e1, c++[c1]) ) (env,[]) (reverse xs)

type Env = M.Map String Int
ev_ :: Env
ev_ = M.fromList []