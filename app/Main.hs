module Main where

import System.Environment
import Compiler
import Interpreter


--TODO Task 3.4
main :: IO ()
main = do
    args <- getArgs
    let coms = read (head args) :: Com
    print(ccomp(coms))