module Main (main) where
import Test.Hspec

import LambdaCalc
import Parser (runProgram, showParse)
import Oleg

main :: IO ()
main = hspec $ 
  do
    lambdaCalcEval
    lambdaCalcParseAndEval
    typechecking

lambdaCalcEval = describe "Lambda Calculus Evaluator" $
  do
    it "should eval 10 to 10" $ eval ten == ten
    it "should allow the trivial Let binding" $ eval (Let "x" ten (Var "x")) == ten
    it "should allow substitution with Let binding" $
      eval (Let "x" ten (Apl identity (Var "x"))) == ten
    it "should allow functions in Let binding" $
      eval (Let "x" identity (Apl (Var "x") ten)) == ten
    it "should add 10 and 10" $
      eval (Apl (Apl plus ten) ten) == Lit (Int_ 20)
    it "should be able to apply identity to a function" $
      eval (Apl (Apl (Apl identity plus) ten) ten) == Lit (Int_ 20)
    it "should be able to apply identity to a partially applied function" $
      eval (Apl (Apl identity (Apl plus ten)) ten) == Lit (Int_ 20)
    it "should be able to apply a lambda" $
      eval (Apl (Lam "y" (Apl (Apl plus ten) (Var "y"))) ten) == Lit (Int_ 20)
    it "should be able to apply incr with a let" $
      eval (Let "incrTen" (Lam "y" (Apl (Apl plus (Var "y")) ten)) (Apl (Var "incrTen") ten)) == Lit (Int_ 20)

lambdaCalcParseAndEval = describe "Parse + Lambda Calculus Eval" $
    do
      it "should parse and eval 10 to 10" $ runProgram "10" == ten
      it "should run a let" $ runProgram "let x = 10 in x" == ten
      it "should run a let with a function and apply it" $ runProgram "let x = \\y -> y in x 10" == ten
      it "should work with builtin operators" $ runProgram "(+) 10 10" == Lit (Int_ 20)
      it "should work with builtin operators" $ runProgram "(-) 10 5" == Lit (Int_ 5)
      it "should work with let and lambdas" $ runProgram "let x = \\y -> (+) 1 y in x 10" == Lit (Int_ 11)
      it "should work with let and lambdas" $ runProgram "let x = \\y -> (+) y 1 in x 10" == Lit (Int_ 11)