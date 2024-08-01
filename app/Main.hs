module Main where

import Parser
import LambdaCalc

main :: IO ()
main = do
  putStrLn "Type an expression to evaluate: "
  putStrLn ""
  input <- getLine
  let
    parsed = showParse input
  putStrLn $ "PARSED: " ++ show parsed
  putStrLn $ "EVALUATED: " ++ show (eval parsed)
  main