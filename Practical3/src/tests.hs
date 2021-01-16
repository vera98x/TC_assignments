module Tests where -- :set -isrc

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

teststring = "void main(){int b;b = 8;char c;c = 'a';// test\n int d;}"
test = "## test\n"

loop = "for(int i, i = 0; i < 5; i = i + 1){int a;}"