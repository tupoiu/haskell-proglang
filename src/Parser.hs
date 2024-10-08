{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use <$>" #-}
{-# HLINT ignore "Eta reduce" #-}
module Parser where

import Text.Megaparsec
import LambdaCalc
import Data.Void
import Debug.Trace

type Parser a = Parsec Void String a
type ParseErr = ParseErrorBundle String Void

term :: Parser Term
term =
      intLit
  <|> try lambda
  <|> try letExpr
  <|> try app
  <|> try builtin
  <|> try bracketed
  <|> try var

applyable :: Parser Term
applyable =
      try builtin
  <|> try bracketed
  <|> try var

-- Operators are defined in LambdaCalc.hs
operators :: [(String, Term)]
operators =
  [ ("(+)", plus)
  , ("(-)", minus)
  , ("id", identity)
  , ("True", Lit (Bool_ True))
  , ("False", Lit (Bool_ False))
  , ("if", ternary)
  ]

builtin :: Parser Term
builtin = go operators empty
  where
    -- Either parse the syntax and return the value or try the next piece of syntax
    go [] parser = parser
    go ((syntax, value):xs) parser = (chunk syntax >> pure value) <|> go xs parser

varName :: Parser Name
varName = do
    name <- some alphaNumChar
    case name of
        "let" -> fail "let is a reserved keyword"
        "in" -> fail "in is a reserved keyword"
        _ -> pure name

var :: Parser Term
var = Var <$> varName

lambda :: Parser Term
lambda = do
    _ <- chunk "\\"
    name <- varName
    _ <- chunk " -> "
    t <- term
    pure $ Lam name t

letExpr :: Parser Term
letExpr = do
    _ <- chunk "let" >> space
    name <- varName
    _ <- space >> chunk "=" >> space
    t1 <- term
    _ <- space >> chunk "in" >> space
    t2 <- term
    pure $ Let name t1 t2

space :: Parser String
space = many $ satisfy (`elem` [' ', '\n', '\t'])

-- For app, we want (+) 1 2 to go to ((+) 1) 2
toApplyTo :: Parser Term
toApplyTo = intLit <|> try builtin <|> try var <|> try bracketed

app :: Parser Term
app = do
    f <- applyable
    applied <- many $ try (chunk " " >> toApplyTo)
    -- _ <- trace ("Applying: " <> show f <> " to " <> show applied) chunk ""
    pure $ go f applied
    where 
      go t [] = t
      go t (x:xs) = go (Apl t x) xs

bracketed :: Parser Term
bracketed = do
    _ <- chunk "("
    t <- term
    _ <- chunk ")"
    pure t

digit :: Parser Char
digit = satisfy (`elem` ['0'..'9'])

alphaNumChar :: Parser Char
alphaNumChar = satisfy (`elem` ['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9'])

intLit :: Parser Term
intLit = Lit . Int_ <$> do
  sign <- chunk "-" <|> chunk ""
  digs <- some digit
  pure $ read (sign <> digs)

parseTerm :: String -> Either ParseErr Term
parseTerm input = parse term "" input

showParse :: String -> Term
showParse input = case parseTerm input of
    Left err -> error $ errorBundlePretty err
    Right x -> x

showParse' :: Show a => Parser a -> String -> a
showParse' parser input = case parse parser "" input of
    Left err -> error $ errorBundlePretty err
    Right x -> x

runProgram :: String -> Term
runProgram = eval . showParse