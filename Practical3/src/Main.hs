module Main where

import System.Environment
import System.FilePath

import ParseLib.Abstract.Derived

import CSharpLex
import CSharpGram
import CSharpAlgebra
import SSM
import CSharpCode
import Prelude hiding ((<$), (<*), (*>))


main :: IO ()
main = do
    -- get command line arguments
    args  <- getArgs
    files <- case args of
              []  ->  do
                putStrLn "no argument given; assuming example.cs"
                return ["example.cs"]
              xs  ->  return xs
    -- translate each of the files
    processFiles files

processFiles :: [FilePath] -> IO ()
processFiles = mapM_ $ processFile2
        . \f -> (f, addExtension (dropExtension f) "ssm")


-- processFile compiles one file; it take the name of the input
-- file and the name of the output file as arguments
processFile :: (FilePath, FilePath) -> IO ()
processFile (infile, outfile) =
  do
    xs <- readFile infile
    writeFile outfile (process xs)
    putStrLn (outfile ++ " written")
  where process = formatCode
                . snd
                . foldCSharp codeAlgebra
                . run (pClass <* eof)
                . run lexicalScanner

processFile2 :: (FilePath, FilePath) -> IO ()
processFile2 (infile, outfile) =
  do
    xs <- readFile infile
    let lex = run lexicalScanner xs
    putStr (show lex)
    putStr "\n\n"
    putStr (show (parse (pClass <* eof) lex))
    putStr "\n\n"
    let parsed = run (pClass <* eof) lex
    putStr (show parsed)
    putStr "\n\n"
    let (ev, code) = foldCSharp codeAlgebra parsed
    putStr (show ev)
    putStr "\n\n"
    let done = formatCode code
    putStr (show done)
    putStr "\n\n"
    writeFile outfile (done)
    putStrLn (outfile ++ " written")


run :: Parser s a -> [s] -> a
run p = fst . head . filter (null . snd) . parse p
