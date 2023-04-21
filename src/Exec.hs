module Exec where

import AM
import Types
import Parser

data ExceptionVal = F | T deriving (Show)

type Assignment = (String, StackVal)
type Stack      = [StackVal]
type State      = ([Assignment], ExceptionVal)
type Config     = (Code, Stack, State)


execute :: Config -> Config
execute ([], [], e)                                 = ([], [], e)
execute (PUSH v : c, s, e)                          = case v of
                                                        (N v) -> (c, N v : s, e)
                                                        (B b) -> (c, B b : s, e)
execute (FETCH x : c, s, e)                         = case lookup x (fst e) of
                                                        Just (N n) -> (c, N n : s, e)
                                                        _ -> error "Variable not found"
execute (STORE x : c, N n : s, e)                   = (c, s, ((x, N n) : filter (\(y, _) -> x /= y) (fst e), snd e))
execute (ADD : c, N (An m) : N (An n) : s, e)       = (c, N (An (m + n)) : s, e)
execute (SUB : c, N (An m) : N (An n) : s, e)       = (c, N (An (m - n)) : s, e)
execute (MUL : c, N (An m) : N (An n) : s, e)       = (c, N (An (m * n)) : s, e)
execute (DIV : c, N (An m) : N (An n) : s, e)       = case n of
                                                        0 -> (c, N Aerr : s, e)
                                                        _ -> (c, N (An (m `div` n)) : s, e)
execute (EQUAL : c, N (An m) : N (An n) : s, e)     = (c, B (Bb (m == n)) : s, e)
execute (LE : c, N (An m) : N (An n) : s, e)        = (c, B (Bb (m <= n)) : s, e)
execute (NEG : c, B (Bb b) : s, e)                  = (c, B (Bb (not b)) : s, e)
execute (AND : c, B (Bb b1) : B (Bb b2) : s, e)     = (c, B (Bb (b1 && b2)) : s, e)
execute (NOOP : c, s, e)                            = (c, s, e)
execute (BRANCH (c1, c2) : c, B (Bb True) : s, e)   = (c1 ++ c, s, e)
execute (BRANCH (c1, c2) : c, B (Bb False) : s, e)  = (c2 ++ c, s, e)
execute (LOOP (c1, c2) : c, s, e)                   = (c1 ++ BRANCH (c2 ++ [LOOP (c1, c2)], [NOOP]) : c, s, e)
execute (TRY ([], c2) : c, s, e)                    = (c, s, e)
execute (TRY (c1, c2) : c, B Berr : s, e)           = (c2 ++ c, s, (fst e, F))
execute (TRY (c1, c2) : c, N Aerr : s ,e)           = (c2 ++ c, s, (fst e, F))
execute (TRY (c1, c2) : c, s, e)                    = (take 1 c1 ++ [TRY (drop 1 c1, c2)] ++ c, s, e)
execute _                                           = error "Invalid Configuration"


executeAMCode :: Code -> State -> Config
executeAMCode c state = execute (c, [], state)