module Exec where

import AM
import Types
import Parser


data StackVal = N Int | B Bool deriving (Show)

type Assignment = (String, Int)
type Stack      = [StackVal]
type State      = [Assignment]
type Config     = (Code, Stack, State)


execute :: Config -> Config
execute ([], [], s)                         = ([], [], s)
execute (PUSH n : c, s, e)                  = execute (c, N (fromIntegral n) : s, e)
execute (FETCH x : c, s, e)                 = case lookup x e of
                                                Just n -> execute (c, N n : s, e)
                                                Nothing -> error $ "Variable not found: " ++ x
execute (STORE x : c, N n : s, e)           = execute (c, s, (x,n) : filter (\(y, _) -> x /= y) e)
execute (ADD : c, N m : N n : s, e)         = execute (c, N (m + n) : s, e)
execute (SUB : c, N m : N n : s, e)         = execute (c, N (m - n) : s, e)
execute (MUL : c, N m : N n : s, e)         = execute (c, N (m * n) : s, e)
execute (DIV : c, N m : N n : s, e)         = execute (c, N (m `div` n) : s, e)
execute (TRUE : c, s, e)                    = execute (c, B True : s, e)
execute (FALSE : c, s, e)                   = execute (c, B False : s, e)
execute (EQUAL : c, N m : N n : s, e)       = execute (c, B (m == n) : s, e)
execute (LE : c, N m : N n : s, e)          = execute (c, B (m <= n) : s, e)
execute (NEG : c, B b : s, e)               = execute (c, B (not b) : s, e)
execute (AND : c, B b1 : B b2 : s, e)       = execute (c, B (b1 && b2) : s, e)
execute (NOOP : c, s, e)                    = execute (c, s, e)
execute (BRANCH c1 c2 : c, B True : s, e)   = execute (c1 ++ c, s, e)
execute (BRANCH c1 c2 : c, B False : s, e)  = execute (c2 ++ c, s, e)
execute (LOOP (c1, c2) : c, s, e)           = execute (c1 ++ BRANCH (c2 ++ [LOOP (c1, c2)]) [NOOP] : c, s, e)
execute (TRY (c1, c2) : c, s, e)            = let (_, s', e') = safeExecution (c1, s, e)
                                                in case lookup "Exception" e' of
                                                    Just n -> execute (c2 ++ c, s', e')
                                                    Nothing -> execute (c, s', e')
-- execute _ = error "Invalid Configuration"
execute cfg = cfg


safeExecution :: Config -> Config
safeExecution ([], s, e) = ([], s, e)
safeExecution (DIV : _, N n : N 0 : s, e) = ([], s, ("Exception", 1) : e)
safeExecution (c, s, e) = let (_, s', e') = execute (take 1 c, s, e)
                            in safeExecution (drop 1 c, s', e')


executeAMCode :: Code -> State -> Config
executeAMCode c state = execute (c, [], state)