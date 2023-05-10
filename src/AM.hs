module AM where

import Types
import Parser

data StackVal = N Avalue | B Bvalue deriving (Show)

-- | Abstract machine 
data Instructions 
    = PUSH StackVal
    | FETCH String
    | STORE String
    | ADD | SUB | MUL | DIV
    | EQUAL | LE | AND | NEG
    | NOOP | CATCH
    | BRANCH (Code, Code)
    | LOOP (Code, Code)
    | TRY (Code, Code)
    deriving (Show)

type Code  = [Instructions]


-- | Compiling AST to Abstract Machine Code
astToCode :: Stm -> Code
astToCode = compileStm

compileAexp :: Aexp -> Code
compileAexp (Aconst n)      = [PUSH (N n)]
compileAexp (Avar x)        = [FETCH x]
compileAexp (Aadd e1 e2)    = compileAexp e2 ++ compileAexp e1 ++ [ADD]
compileAexp (Asub e1 e2)    = compileAexp e2 ++ compileAexp e1 ++ [SUB]
compileAexp (Amul e1 e2)    = compileAexp e2 ++ compileAexp e1 ++ [MUL]
compileAexp (Adiv e1 e2)    = compileAexp e2 ++ compileAexp e1 ++ [DIV]

compileBexp :: Bexp -> Code
compileBexp (Bconst b)      = [PUSH (B b)]
compileBexp (Beq e1 e2)     = compileAexp e2 ++ compileAexp e1 ++ [EQUAL]
compileBexp (Bleq e1 e2)    = compileAexp e2 ++ compileAexp e1 ++ [LE]
compileBexp (Bneg b)        = compileBexp b ++ [NEG]
compileBexp (Band b1 b2)    = compileBexp b2 ++ compileBexp b1 ++ [AND]

compileStm :: Stm -> Code
compileStm (Ass x e)        = compileAexp e ++ [STORE x]
compileStm Skip             = [NOOP]
compileStm (Comp s1 s2)     = compileStm s1 ++ compileStm s2
compileStm (If b s1 s2)     = compileBexp b ++ [BRANCH (compileStm s1, compileStm s2)]
compileStm (While b s)      = [LOOP (compileBexp b, compileStm s)]
compileStm (Try s1 s2)      = [TRY (compileStm s1, compileStm s2)]
