module Lexer (
  Operator(..),
  Token(..),
  tokenize
  )
  where

import Data.Char
data Operator = Plus | Minus | Times | Div
     deriving (Show, Eq)

data Token = TokOp Operator
           | TokIdent String
           | TokNum Double
           | TokSpace
           | TokAssign
           | TokLParen
           | TokRParen
           | TokEnd
    deriving (Show, Eq)

operator :: Char -> Operator
operator c | c == '+' = Plus
           | c == '-' = Minus
           | c == '*' = Times
           | c == '/' = Div

tokenize :: String -> [Token]
tokenize [] = []
tokenize (c:cs)
  | elem c "+-*/" = TokOp (operator c) : tokenize cs
  | c == '(' = TokLParen : tokenize cs
  | c == ')' = TokRParen : tokenize cs
  | c == '=' = TokAssign : tokenize cs
  | isDigit c = number c cs
  | isAlpha c = identifier c cs
  | isSpace c = tokenize cs
  | otherwise = error $ "Cannot tokenize :: String -> [Token]"

identifier c cs = let (str, cs') = span isAlphaNum cs in
                    TokIdent (c:str) : tokenize cs'

number c cs =
  let (digs, cs') = span isDigit cs in
    TokNum (read (c:digs)) : tokenize cs'
