module Tests where

import CSharpLex
import CSharpGram
import CSharpAlgebra
import SSM
import CSharpCode

import Data.Char
import Control.Monad hiding ((<$))
import ParseLib.Abstract
import Prelude hiding ((<$), (<*), (*>))

teststring = "void main(){int b;b = 8;char c;c = 'a';// test\n int d;}"
test = "## test\n"