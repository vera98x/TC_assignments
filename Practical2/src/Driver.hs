module Driver where

import Algebra
import Model
import Interpreter
import ParseLib.Abstract -----delete later


main3 = do
       s <- readFile "../examples/Maze.space"
       let (space:ss) = ParseLib.Abstract.parse parseSpace s
       a <- readFile "../examples/Add.arrow"
       let env = (toEnvironment a)
       putStr ("\n" ++ (show env) ++ "\n\n")
       let as = ArrowState (fst space) (0,0) HRight (Cmds [(CASE RIGHT 
                                                            (Alts [(Alt LAMBDA (Cmds([GO, GO, GO, GO,GO, MARK])))])
                                                          )])
       return (interactive env as)

-- Exercise 11
interactive :: Environment -> ArrowState -> IO ()
interactive env as = do
                     let newStep = (step env as)
                     case newStep of
                       (Fail s) -> putStr (s)
                       (Done s p h) -> putStr (printSpace(s) ++ "Done!\n" )
                       (Ok newAs@(ArrowState s p h st)) -> let _ = putStr (printSpace s)
                                                               x = getLine in
                                                               interactive env newAs
                     return ()

batch :: Environment -> ArrowState -> (Space, Pos, Heading)
batch = undefined