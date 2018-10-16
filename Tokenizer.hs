module Tokenizer where

data Operator = Plus
              | Minus
              | Mult
              | Div
              | Power
              | Cmd
              | Comma
              | Concat
              deriving (Show, Eq)

operator :: Char -> Operator
operator c | c == '+' = Plus
           | c == '-' = Minus
           | c == '*' = Mult
           | c == '/' = Div
           | c == '^' = Power
           | c == ';' = Cmd
           | c == ',' = Comma
operator c = error ("Lexical error: " ++ c : " is not an operator!")

concat_op :: String -> Operator
concat_op s
  | s == "++" = Concat
  | otherwise = error ("Lexical error: " ++ s ++ " is not an concat operator!")

isDigit :: Char -> Bool
isDigit x = x `elem` "0123456789"

digit :: Char -> Integer
digit c | c == '0' = 0
        | c == '1' = 1
        | c == '2' = 2
        | c == '3' = 3
        | c == '4' = 4
        | c == '5' = 5
        | c == '6' = 6
        | c == '7' = 7
        | c == '8' = 8
        | c == '9' = 9
digit c = error ("Lexical error: " ++ c : " is not a digit!")

value :: String -> Integer
value [] = 0
value s = read s :: Integer

isValue :: String -> Bool
isValue [] = False
isValue s = fst (span isDigit s) == s

isAlpha :: Char -> Bool
isAlpha c = c `elem` ['a' .. 'z']

alpha :: Char -> Char
alpha c = c

varname :: String -> String
varname [] = ""
varname s = x ++ y
  where
    (x, rest) = span (\x -> (isAlpha x) || (x == '_')) s
    (y, _) = span (\x -> (isAlpha x) || (x == '_') || (isDigit x)) rest

isVarname :: String -> Bool
isVarname [] = False
isVarname (c:cs) 
  | not (isAlpha c || c == '_') = False
  | otherwise = fst (span (\x -> (isAlpha x) || (x == '_') || (isDigit x)) cs) == cs

isWhiteSpace :: Char -> Bool
isWhiteSpace c = c `elem` " \t\n"

isConcatOp :: String -> Bool
isConcatOp s = s == "++"
