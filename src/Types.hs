module Types where

data Avalue = An Integer | Aerr deriving (Show)
data Bvalue = Bb Bool | Berr deriving (Show)

data Aexp
    = Aconst Avalue
    | Avar String 
    | Aadd Aexp Aexp
    | Asub Aexp Aexp
    | Amul Aexp Aexp
    | Adiv Aexp Aexp
    deriving (Show)

data Bexp
    = Bconst Bvalue
    | Beq Aexp Aexp
    | Bleq Aexp Aexp
    | Bneg Bexp
    | Band Bexp Bexp
    deriving (Show)

data Stm
    = Ass String Aexp 
    | Skip
    | Comp Stm Stm
    | If Bexp Stm Stm
    | While Bexp Stm
    | Try Stm Stm
    deriving (Show)