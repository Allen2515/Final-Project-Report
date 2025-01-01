module Eval where

import Text.Parsec
import Text.Parsec.String (Parser)
import Text.Parsec.Expr
import Data.Functor.Identity
import Text.Parsec.Token
import Text.Parsec.Language (haskellDef)

lexer :: TokenParser ()
lexer = makeTokenParser haskellDef

number :: Parser Double
number = do
  num <- many1 digit 
  return (read num)

floating :: Parser Double
floating = float lexer

table :: OperatorTable String () Identity Double
table = [
        [ Infix (try (char '*' >> return (*))) AssocLeft,
          Infix (try (char '/' >> return (/))) AssocLeft ],
        [ Infix (try (char '+' >> return (+))) AssocLeft,
          Infix (try (char '-' >> return (-))) AssocLeft ]
        ]

term :: Parser Double
term = try floating <|> number

expr :: Parser Double
expr = buildExpressionParser table term
  

eval :: String -> Maybe Double
eval input = case parse expr "" input of
  Left _ -> Nothing
  Right result -> Just result
