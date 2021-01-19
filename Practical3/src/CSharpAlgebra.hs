module CSharpAlgebra where

import CSharpLex
import CSharpGram

{-
  Only modify this file when you change the AST in CSharpGram.hs
-}

type CSharpAlgebra clas memb stat expr                --
    = (  Token -> [memb] -> clas                      --  Class = Class Token [Member]
                                                      --
      ,  ( Decl                             -> memb   --  Member = MemberD Decl
         , Type -> Token -> [Decl] -> stat  -> memb   --         | MemberM Type Token [Decl] Stat
         )                                            --
                                                      --
      ,  ( Decl                  -> stat              --  Stat = StatDecl   Decl
         , expr                  -> stat              --       | StatExpr   Expr
         , expr -> stat -> stat  -> stat              --       | StatIf     Expr Stat Stat
         , expr -> stat          -> stat              --       | StatWhile  Expr Stat
         , [stat]->expr->[stat] -> stat -> stat       --       | StatFor    [Stat] Expr [Stat] Stat
         , expr                  -> stat              --       | StatReturn Expr
         , Token -> [expr]       -> stat              --       | StatCall Expr
         , [stat]                -> stat              --       | StatBlock  [Stat]
         )                                            --
                                                      --
      ,  ( Token                  -> expr             --  Expr = ExprConst  Token
         , Token                  -> expr             --       | ExprVar    Token
         , Token -> expr -> expr  -> expr             --       | ExprOper   Token Expr Expr
         )                                            --
      )


foldCSharp :: CSharpAlgebra clas memb stat expr -> Class -> clas
foldCSharp (c, (md,mm), (sd,se,si,sw,sf,sr,sc,sb), (ec,ev,eo)) = fClas
    where
        fClas (Class      t ms)     = c  t (map fMemb ms)
        fMemb (MemberD    d)        = md d
        fMemb (MemberM    t m ps s) = mm t m ps (fStat s)
        fStat (StatDecl   d)        = sd d
        fStat (StatExpr   e)        = se (fExpr e)
        fStat (StatIf     e s1 s2)  = si (fExpr e) (fStat s1) (fStat s2)
        fStat (StatWhile  e s1)     = sw (fExpr e) (fStat s1)
        fStat (StatFor ss1 e ss2 s) = sf (map fStat ss1) (fExpr e) (map fStat ss2) (fStat s)
        fStat (StatReturn e)        = sr (fExpr e)
        fStat (StatCall t es)       = sc t (map fExpr es)
        fStat (StatBlock  ss)       = sb (map fStat ss)
        fExpr (ExprConst  con)      = ec con
        fExpr (ExprVar    var)      = ev var
        fExpr (ExprOper   op e1 e2) = eo op (fExpr e1) (fExpr e2)

