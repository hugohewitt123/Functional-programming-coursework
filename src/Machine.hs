module Machine
(      
        Vname,
        Val,
        State,
        Instr (..),
        Stack,
        Config,
        iexec,
        exec
) where

import Data.Map

--TODO Task 1.1
type Vname = (String)
--TODO Task 1.2
type Val = (Int)
--TODO Task 1.3
type State = (Map Vname Val)

--TODO Task 1.4
data Instr = LOADI Int
        | LOAD String
        | ADD
        | STORE String
        | JMP Int
        | JMPLESS Int
        | JMPGE Int
        deriving (Eq, Read, Show)

--TODO Task 1.5
type Stack = [Int]

--TODO Task 1.6
type Config = (Int, State, Stack)

--TODO Task 1.7
iexec :: Instr -> Config -> Config
iexec (LOADI a) (b, c, xs) = (b+1, c, xs ++ [a])
iexec (LOAD a) (b, c, xs) = (b+1, c, xs ++ [findWithDefault (-1) a c])
iexec (ADD) (b, c, d:e:xs) = (b+1, c, xs ++ [d+e])
iexec (STORE a) (b, c, d:xs) = (b+1, fromList ((toList c) ++ [(a,d)]), xs)
iexec (JMP a) (b, c, xs) = (b+a+1, c, xs)
iexec (JMPLESS a) (b, c, d:e:xs) = ((if d > e then b+a+1 else b+1), c, xs)
iexec (JMPGE a) (b, c, d:e:xs) = ((if d <= e then b+a+1 else b+1), c, xs)


--TODO Task 1.8
exec :: [Instr] -> Config -> Config
exec [] (b, c, xs) = (b, c, xs)
exec (a:as) (b, c, xs) = exec as (iexec (a) (b, c, xs))