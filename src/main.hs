module Main where 

import Parser 
import Types
import AM

import System.IO
import Exec (executeAMCode)


loadFile :: FilePath -> IO Stm
loadFile path = do
    input <- readFile path
    return $ parseString input

loadStdin :: IO Stm
loadStdin = do parseString <$> getContents


main :: IO ()
main = do
    let input = "x:=7;try x:=x-7;x:=7/x;x:=x+7 catch x:=x-7"
    putStrLn "Input:"
    print input
    runProgram input


runProgram :: String -> IO ()
runProgram programString = do
    let ast = parseString programString
    putStrLn "AST:"
    print ast
    putStrLn "AM Code:"
    let code = astToCode ast
    print code
    putStrLn "Final Config:"
    let finalConfig = executeAMCode code []
    print finalConfig
