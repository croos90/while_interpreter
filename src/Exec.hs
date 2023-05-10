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
execute (CATCH : c, s, (v, T))                      = (c, [B (Bb True)], (v, T))
execute (CATCH : c, s, (v, F))                      = (c, [B (Bb False)], (v, T))
execute (_ : c, s, (v, F))                          = (c , [], (v, F))
execute (NOOP : c, s, e)                            = (c, s, e)
execute (PUSH v : c, s, e)                          = (c, v : s, e)
execute (FETCH x : c, s, e)                         = case lookup x (fst e) of
                                                        Just (N n) -> (c, N n : s, e)
                                                        _ -> error "Variable not found"
execute (STORE x : c, N (An n) : s, (v, T))         = (c, s, ((x, N (An n)) : filter (\(y, _) -> x /= y) v, T))
execute (STORE x : c, N Aerr : s, (v, _))           = (c, s, (v, F))

execute (ADD : c, N (An m) : N (An n) : s, e)       = (c, N (An (m + n)) : s, e)
execute (SUB : c, N (An m) : N (An n) : s, e)       = (c, N (An (m - n)) : s, e)
execute (MUL : c, N (An m) : N (An n) : s, e)       = (c, N (An (m * n)) : s, e)
execute (DIV : c, N (An m) : N (An 0) : s, e)       = (c, N Aerr : s, e)
execute (DIV : c, N (An m) : N (An n) : s, e)       = (c, N (An (m `div` n)) : s, e)

execute (EQUAL : c, N (An m) : N (An n) : s, e)     = (c, B (Bb (m == n)) : s, e)
execute (LE : c, N (An m) : N (An n) : s, e)        = (c, B (Bb (m <= n)) : s, e)
execute (NEG : c, B (Bb b) : s, e)                  = (c, B (Bb (not b)) : s, e)
execute (AND : c, B (Bb b1) : B (Bb b2) : s, e)     = (c, B (Bb (b1 && b2)) : s, e)

execute (ADD : c, _ : _ : s, e)                     = (c, N Aerr : s, e)
execute (SUB : c, _ : _ : s, e)                     = (c, N Aerr : s, e)
execute (MUL : c, _ : _ : s, e)                     = (c, N Aerr : s, e)
execute (DIV : c, _ : _ : s, e)                     = (c, N Aerr : s, e)
execute (EQUAL : c, _ : _ : s, e)                   = (c, B Berr : s, e)
execute (LE : c, _ : _ : s, e)                      = (c, B Berr : s, e)
execute (NEG : c, _ : s, e)                         = (c, B Berr : s, e)
execute (AND : c, _ : _ : s, e)                     = (c, B Berr : s, e)

execute (BRANCH (c1, c2) : c, B (Bb True) : s, e)   = (c1 ++ c, s, e)
execute (BRANCH (c1, c2) : c, B (Bb False) : s, e)  = (c2 ++ c, s, e)
execute (LOOP (c1, c2) : c, s, e)                   = (c1 ++ BRANCH (c2 ++ [LOOP (c1, c2)], [NOOP]) : c, s, e)
execute (TRY (c1, c2) : c, s, e)                    = (c1 ++ [CATCH] ++ [BRANCH ([NOOP], c2)], s, e)
execute _                                           = error "Invalid Configuration"


executeAMCode :: Code -> State -> Config
executeAMCode c state = execute (c, [], state)
