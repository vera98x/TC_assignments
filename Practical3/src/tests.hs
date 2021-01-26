module Tests where -- :set -isrc -- ssm.bat example.ssm

import CSharpLex
import CSharpGram
import CSharpAlgebra
import SSM
import CSharpCode
import Main

import Data.Char
import Control.Monad hiding ((<$))
import ParseLib.Abstract
import Prelude hiding ((<$), (<*), (*>))

teststring = "class Hello{int g;void main(){int b;b = 8;char c;c = 'a';// test\n int d; main(8,7);}}"

loop = "for(int i, i = 0; i < 5; i = i + 1){int a;}"

order = parse pExpr $ run lexicalScanner "9*(2*6+7)"

parsing x = parse pClass (run lexicalScanner x)
test = "class Hello{int g;void main(){main(8,7);}}"

lexed = "MemberM TypeVoid (LowerId \"main\") [] (StatBlock [StatDecl (Decl (TypePrim (StdType \"int\")) (LowerId \"a\")),StatDecl (Decl (TypePrim (StdType \"int\")) (LowerId \"c\")),StatExpr (ExprOper (Operator \"=\") (ExprVar (LowerId \"c\")) (ExprConst (ConstInt 1))),StatExpr (ExprOper (Operator \"=\") (ExprVar (LowerId \"a\")) (ExprOper (Operator \"+\") (ExprOper (Operator \"+\") (ExprOper (Operator \"+\") (ExprConst (ConstInt 8)) (ExprOper (Operator \"*\") (ExprConst (ConstInt 14)) (ExprConst (ConstInt 18)))) (ExprConst (ConstInt 6))) (ExprOper (Operator \"/\") (ExprConst (ConstInt 4)) (ExprConst (ConstInt 8))))),StatCall (LowerId \"print\") [ExprVar (LowerId \"c\")]])"

--lexs = "[KeyClass,UpperId \"Hello\",COpen,StdType \"int\",LowerId \"g\",Semicolon,KeyVoid,LowerId \"main\",POpen,PClose,COpen,ConstInt 4,Operator \"+\",ConstInt 4,Semicolon,StdType \"int\",LowerId \"b\",Semicolon,LowerId \"square\",POpen,ConstInt 5,Comma,ConstInt 2,Comma,ConstInt 7,PClose,Semicolon,LowerId \"print\",POpen,LowerId \"b\",PClose,Semicolon,CClose,StdType \"int\",LowerId \"square\",POpen,StdType \"int\",LowerId \"x\",Comma,StdType \"int\",LowerId \"y\",Comma,StdType \"int\",LowerId \"z\",PClose,COpen,LowerId \"print\",POpen,LowerId \"x\",PClose,Semicolon,LowerId \"print\",POpen,LowerId \"y\",PClose,Semicolon,LowerId \"print\",POpen,LowerId \"z\",PClose,Semicolon,LowerId \"print\",POpen,LowerId \"x\",Operator \"*\",LowerId \"y\",PClose,Semicolon,KeyReturn,LowerId \"x\",Semicolon,CClose,CClose]"
lexs = "class Hello{int g;void main(){4 + 4;int b;4+4;print(1);}}"
first = run lexicalScanner lexs
testing = parse pClass first


