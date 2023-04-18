module Types where

data Aexp
    = Aconst Integer
    | Avar String 
    | Aadd Aexp Aexp
    | Asub Aexp Aexp
    | Amul Aexp Aexp
    | Adiv Aexp Aexp
    deriving (Show, Eq)

data Bexp
    = Btrue
    | Bfalse
    | Beq Aexp Aexp
    | Bleq Aexp Aexp
    | Bneg Bexp
    | Band Bexp Bexp
    deriving (Show, Eq)

data Stm
    = Ass String Aexp 
    | Skip
    | Comp Stm Stm
    | If Bexp Stm Stm
    | While Bexp Stm
    | Try Stm Stm
    deriving (Show, Eq)