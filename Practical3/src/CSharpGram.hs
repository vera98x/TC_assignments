module CSharpGram where

import ParseLib.Abstract hiding (braced, bracketed, parenthesised)
import CSharpLex
import Prelude hiding ((<$), (<*), (*>))

data Class = Class Token [Member]
    deriving Show

data Member = MemberD Decl
            | MemberM Type Token [Decl] Stat
            deriving Show

data Stat = StatDecl   Decl
          | StatExpr   Expr
          | StatIf     Expr Stat Stat
          | StatWhile  Expr Stat
          | StatFor    [Stat] Expr [Stat] Stat
          | StatReturn Expr
          | StatCall   Token [Expr]
          | StatBlock  [Stat]
          deriving Show

data Expr = ExprConst  Token
          | ExprVar    Token
          | ExprOper   Token Expr Expr
          deriving Show

data Decl = Decl Type Token
    deriving Show

data Type = TypeVoid
          | TypePrim  Token
          | TypeObj   Token
          | TypeArray Type
          deriving (Eq,Show)


pClass :: Parser Token Class
pClass = Class <$ symbol KeyClass <*> sUpperId <*> braced (many pMember)

pMember :: Parser Token Member
pMember =  MemberD <$> pDeclSemi
       <|> pMeth

pMeth :: Parser Token Member
pMeth = MemberM <$> methRetType <*> sLowerId <*> methArgList <*> pBlock
    where
        methRetType = pType <|> TypeVoid <$ symbol KeyVoid
        methArgList = parenthesised (option (listOf pDecl (symbol Comma)) [])

pBlock :: Parser Token Stat
pBlock = StatBlock <$> braced (many pStatDecl)

pStatDecl :: Parser Token Stat
pStatDecl =  pStat
         <|> StatDecl <$> pDeclSemi

pCSStatDecl :: Parser Token Stat
pCSStatDecl = StatDecl <$> pDecl
         <|>  StatExpr <$> pExpr

pCSStatDecls :: Parser Token [Stat]
pCSStatDecls =  ((:) <$> pCSStatDecl <*> (greedy (symbol Comma *> pCSStatDecl)) <<|> succeed [])

pStat :: Parser Token Stat
pStat =  StatExpr   <$> pExpr <*  sSemi
     <|> StatIf     <$ symbol KeyIf     <*> parenthesised pExpr <*> pStat <*> optionalElse
     <|> StatWhile  <$ symbol KeyWhile  <*> parenthesised pExpr <*> pStat
     <|> StatFor    <$> (symbol KeyFor   *> ( symbol POpen *> pCSStatDecls <* sSemi)) <*> (pExpr <* sSemi) <*> (pCSStatDecls <* (symbol PClose)) <*> pStat
     <|> StatReturn <$ symbol KeyReturn <*> pExpr               <*  sSemi
     <|> StatCall   <$> sLowerId <*> parenthesised (option (listOf pExpr (symbol Comma)) []) <* sSemi
     <|> pBlock
     where optionalElse = option (symbol KeyElse *> pStat) (StatBlock [])

pExpr, pExpr2, pExpr3, pExpr4, pExpr5, pExpr6, pExpr7, pExpr8, pExprSimple :: Parser Token Expr
pExpr  = chainr pExpr2 op1
pExpr2 = chainl pExpr3 op2
pExpr3 = chainl pExpr4 op3
pExpr4 = chainl pExpr5 op4
pExpr5 = chainl pExpr6 op5
pExpr6 = chainl pExpr7 op6
pExpr7 = chainl pExpr8 op7
pExpr8 = chainl pExprSimple op8
pExprSimple =  ExprConst <$> sConst
           <|> ExprVar   <$> sLowerId
           <|> parenthesised pExpr

op1, op2, op3, op4, op5, op6, op7, op8 :: Parser Token (Expr->Expr->Expr)
op1 = ExprOper <$> symbol (Operator "=")
op2 = ExprOper <$> symbol (Operator "||")
op3 = ExprOper <$> symbol (Operator "&&")
op4 = ExprOper <$> symbol (Operator "^")
op5 = ExprOper <$> symbol (Operator "==") <|> ExprOper <$> symbol (Operator "!=")
op6 = ExprOper <$> symbol (Operator "<=") <|> ExprOper <$> symbol (Operator "<") <|> ExprOper <$> symbol (Operator ">") <|> ExprOper <$> symbol (Operator "=>")
op7 = ExprOper <$> symbol (Operator "+")  <|> ExprOper <$> symbol (Operator "-")
op8 = ExprOper <$> symbol (Operator "*")  <|> ExprOper <$> symbol (Operator "/") <|> ExprOper <$> symbol (Operator "%")


pDecl :: Parser Token Decl
pDecl = Decl <$> pType <*> sLowerId

pDeclSemi :: Parser Token Decl
pDeclSemi = pDecl <* sSemi

pType :: Parser Token Type
pType = foldr (const TypeArray) <$> pType0 <*> many (bracketed (succeed ()))

pType0 :: Parser Token Type
pType0 =  TypePrim <$> sStdType
      <|> TypeObj  <$> sUpperId


-- The `Token` equivalents to some basic parser combinators
parenthesised, bracketed, braced :: Parser Token b -> Parser Token b
parenthesised p = pack (symbol POpen) p (symbol PClose) --(p)
bracketed     p = pack (symbol SOpen) p (symbol SClose) --[p]
braced        p = pack (symbol COpen) p (symbol CClose) --{p}
