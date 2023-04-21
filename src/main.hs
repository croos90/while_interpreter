module Main where 

import System.Environment
import System.IO

import Parser
import Types
import AM
import Exec


main :: IO ()
main = do
    args <- getArgs
    let inputFile = head args
    contents <- readFile inputFile
    let input = head (lines contents)
--    let input = "if !(1=2) then skip else x:=2"
    putStrLn "Input:"
    print input
    putStrLn ""
    let ast = parseString input
    putStrLn "AST:"
    print ast
    putStrLn ""
    let code = astToCode ast
    putStrLn "AM Code:"
    print code
    putStrLn ""
    putStrLn "Enter 'r' to run the program or 'd' to enter the debugger:"
    choice <- getChar
    putStrLn ""
    putStrLn ""
    if choice == 'r' then
        runProgram (code, [], ([],T))
    else if choice == 'd' then
        debugger (code, [], ([],T))
    else
        putStrLn "Invalid choice."


debugger :: Config -> IO ()
debugger config@(code, stack, state) = do
    putStrLn $ "Code: " ++ show code
    putStrLn $ "Stack: " ++ show stack
    putStrLn $ "State: " ++ show state
    getLine
    let newConfig@(newCode, newStack, newState) = execute config
    if null newCode && null newStack
        then do
            putStrLn "Execution finished."
            putStrLn $ "Code: " ++ show newCode
            putStrLn $ "Stack: " ++ show newStack
            putStrLn $ "Final state: " ++ show newState
        else debugger newConfig


runProgram :: Config -> IO ()
runProgram config@(code, stack, state) = loop config
  where
    loop cfg@(code, stack, state) = do
        print state
        let newConfig@(newCode, newStack, newState) = execute cfg
        if null newCode && null newStack
            then do
                print newState
                putStrLn "Execution finished."                
            else loop newConfig
