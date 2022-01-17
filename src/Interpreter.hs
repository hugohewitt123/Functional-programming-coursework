module Interpreter
(
    AExp(..),
    BExp(..),
    Com (..),
    aval,
    bval,
    eval
) where

import Data.Map
import Machine

--TODO Task 2.1
data AExp = N Int
    | V String
    | Plus AExp AExp
    deriving (Eq, Read, Show)

--TODO Task 2.2
aval :: AExp -> State -> Val
aval (N a) (b) = a
aval (V a) (b) = findWithDefault (-1) a b
aval (Plus a b) (c) = aval a c + aval b c

--TODO Task 2.1
data BExp = Bc Bool
    | Not BExp
    | And BExp BExp
    | Less AExp AExp
    deriving (Eq, Read, Show)

--TODO Task 2.3
bval :: BExp -> State -> Bool
bval (Bc a) (b) = a
bval (Not a) (b) = if bval a b == True then False else True
bval (And a b) (c) = if bval a c == True && bval b c == True then True else False
bval (Less a b) (c) = if aval a c < aval b c then True else False

--TODO Task 2.1
data Com = Assign Vname AExp
    | Seq Com Com
    | If BExp Com Com
    | While BExp Com
    | SKIP
    deriving (Eq, Read, Show)

--TODO Task 2.4
eval :: Com -> State -> State
eval (Assign a b) (c) = fromList ((toList c) ++ [(a, aval b c)])
eval (Seq a b) (c) = (eval b (eval a c))
eval (If a b c) (d) = if bval a d == True then eval b d else eval c d
eval (While a b) (c) = if bval a c == True then eval (While a b) (eval b c) else c
eval (SKIP) (c) = c