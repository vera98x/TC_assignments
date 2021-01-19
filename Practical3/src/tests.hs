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

methCall = parse pStat $ run lexicalScanner "main(8+7);"

parsing x = parse pClass (run lexicalScanner x)
test = "class Hello{int g;void main(){main(8,7);}}"