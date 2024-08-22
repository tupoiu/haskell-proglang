module Main where

import Parser
import LambdaCalc

main :: IO ()
main = do
  prelude <- readFile "Prelude.txt"
  putStrLn "Type an expression to evaluate: "
  putStrLn ""
  input <- getLine
  let
    parsed = showParse input
  putStrLn $ "PARSED: " ++ show parsed
  putStrLn $ "EVALUATED: " ++ show (runProgram $ prelude <> "\n" <> input)
  main