module Driver where

import Algebra
import Model
import Interpreter
import ParseLib.Abstract 
import Parser            
import Lexer            

-- Exercise 11

printStatement :: Step -> IO()
printStatement (Fail s) = putStr ("Fail: " ++ s)
printStatement (Done s p h) = putStr (printSpace(s) ++ "Done!\n" )
printStatement (Ok newAs@(ArrowState s p h st)) = putStr (printSpace s)

interactive :: Environment -> ArrowState -> IO ()
interactive env as = do
                     let newStep = (step env as)
                     _ <- printStatement newStep
                     case newStep of
                       (Ok newAs@(ArrowState s p h st)) -> do getLine 
                                                              interactive env newAs
                       _ -> return () -- do nothing if not ok. Result is printed already, nothing has to be done.

batch :: Environment -> ArrowState -> (Space, Pos, Heading)
batch env as = do
              let newStep = (step env as)
              case newStep of
                     (Ok newAs@(ArrowState s p h st)) -> do 
                                                         batch env newAs
                     (Done s p h) -> (s, p, h)
                     (Fail s) -> error s

interactiveAddMain :: IO()
interactiveAddMain = do
       s <- readFile "../examples/AddInput.space"
       let (space:ss) = ParseLib.Abstract.parse parseSpace s
       a <- readFile "../examples/Add.arrow"
       let env = (toEnvironment a)
       let (Program ((Rule s cmds) : xs )) = Parser.parseTokens (alexScanTokens a)
       putStr ((show cmds) ++ "\n")
       let as = ArrowState (fst space) (0,0) East cmds -- add the first command to arrowstate, that is start
       interactive env as

batchAddMain :: IO()
batchAddMain = do
       s <- readFile "../examples/AddInput.space"
       let (space:ss) = ParseLib.Abstract.parse parseSpace s
       a <- readFile "../examples/Add.arrow"
       let env = (toEnvironment a)
       let (Program ((Rule s cmds) : xs )) = Parser.parseTokens (alexScanTokens a)
       let as = ArrowState (fst space) (0,0) East cmds -- add the first command to arrowstate, that is start
       let (s, p, h) = batch env as
       putStr (printSpace s)

main_ :: IO()
main_ = do
       putStr "Which spacefile do you want to use?"
       spaceInput <- getLine
       s <- readFile spaceInput --"../examples/AddInput.space"
       let (space:ss) = ParseLib.Abstract.parse parseSpace s
       putStr "Which arrowfile do you want to use?"
       arrowInput <- getLine
       a <- readFile arrowInput --"../examples/Add.arrow"
       let env = (toEnvironment a)
       let (Program ((Rule s cmds) : xs )) = Parser.parseTokens (alexScanTokens a)
       putStr "Which x position do you want to use?"
       x_ <- getLine
       let x = read x_ :: Int
       putStr "Which y position do you want to use?"
       y_ <- getLine 
       let y = read y_ :: Int 
       putStr "Which heading do you want to use: e, n, w, s (default east)?"
       h <-  getLine
       let heading = case h of
                  "e" -> East
                  "n" -> North
                  "w" -> West
                  "s" -> South
                  _  -> East
       let as = ArrowState (fst space) (x,y) heading (cmds)
       putStr "Do you want to use batch mode: y, n (default no)?"
       res <-  getLine
       if res == "y" then do
              let (s, p, h) = batch env as
              putStr (printSpace s)
       else interactive env as
       