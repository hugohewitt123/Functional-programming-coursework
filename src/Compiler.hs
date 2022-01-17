module Compiler
(
    acomp,
    bcomp,
    ccomp
) where

import Machine
import Interpreter

--TODO Task 3.1
acomp :: AExp -> [Instr]
acomp (N a) = [LOADI a]
acomp (V a) = [LOAD a]
acomp (Plus a b) = acomp a ++ acomp b ++ [ADD]

--TODO Task 3.2
bcomp :: BExp -> Bool -> Int -> [Instr]
bcomp (Bc a) (b) (c) = if a == b then [JMP c] else []
bcomp (Not a) (b) (c) = if bcomp a b c == [JMP c] then [] else [JMP c]
bcomp (And a b) (c) (d) = if bcomp b c d /= [] then 
                                if c == True then bcomp a (not c) (length (bcomp b c d)) ++ bcomp b c d
                                else bcomp a c (d + length (bcomp b c d)) ++ bcomp b c d
                          else if bcomp b c d == [] && c == False then (bcomp a c d)
                          else []
bcomp (Less a b) (c) (d) = if c == True then acomp a ++ acomp b ++ [JMPLESS d]
                           else acomp a ++ acomp b ++ [JMPGE d]

--TODO Task 3.3
ccomp :: Com -> [Instr]
ccomp (Assign a b) = acomp b ++ [STORE a]
ccomp (Seq a b) = ccomp a ++ ccomp b
ccomp (If a b c) = bcomp a False (length (ccomp b)+1) ++ ccomp b ++ [JMP (length (ccomp c))] ++ ccomp c
ccomp (While a b) = bcomp a False (length (ccomp b)+1) ++ ccomp b ++ [JMP (-length (ccomp (While a b)))]
