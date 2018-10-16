module Combinators where
-- Make sure that the names don't clash
import Prelude hiding (lookup, (>>=), map, pred, return, elem)

import Data.Char

-- Input abstraction
type Input = String

-- Result is polymorphic in the ... result
data Result r = Success r
              | Error String
              deriving (Show)

-- The result of parsing is some payload r and the suffix which wasn't parsed
type Parser r = Input -> Result (r, Input)

finish :: Parser r -> Input -> Result r
finish parser input = 
  case (parser input) of
    Success (r, []) -> Success r
    Success (r, ts) -> Error ("Syntax error on: " ++ show ts)
    Error err -> Error err

-- Choice combinator: checks if the input can be parsed with either the first, or the second parser
-- Left biased: make sure, that the first parser consumes more input
infixl 6 <|>
(<|>) :: Parser a -> Parser a -> Parser a
p <|> q = \inp ->
  case p (dropWhile isSpace inp) of
    Error _ -> q inp
    result  -> result

-- Sequential combinator: if the first parser successfully parses some prefix, the second is run on the suffix
-- The second parser is supposed to use the result of the first parser
infixl 7 >>=
(>>=) :: Parser a -> (a -> Parser b ) -> Parser b
p >>= q = \inp ->
  case p (dropWhile isSpace inp) of
    Success (r, inp') -> q r inp'
    Error err -> Error err

-- Sequential combinator which ignores the result of the first parser
infixl 7 |>
(|>) :: Parser a -> Parser b -> Parser b
p |> q = p >>= const q

-- Succeedes without consuming any input, returning a value
return :: a -> Parser a
return r inp = Success (r, dropWhile isSpace inp)

-- Always fails
zero :: String -> Parser a
zero err = const $ Error err

-- Chops of the first element of the string
elem :: Parser Char
elem (c : cs) = Success (c, cs)
elem [] = Error "Empty string"

elem' :: (Char -> Bool) -> Parser String
elem' pred inp
  | s /= [] = Success(s, rest)
  | otherwise = Error "Can't read symbol"
  where
    (s, rest) = span pred (dropWhile isSpace inp)

-- Checks if the first character of the string is the given one
char :: Char -> Parser Char
char c = sat (== c) elem

-- Checks if the parser result satisfies the predicate
sat :: (a -> Bool) -> Parser a -> Parser a
sat pred parser inp =
  case parser (dropWhile isSpace inp) of
    Success (r, inp') | pred r ->  Success (r, inp')
    Success _ -> Error "Predicate is not satisfied"
    Error err -> Error err

-- Applies the function to the result of the parser
map :: (a -> b) -> Parser a -> Parser b
map f parser inp =
  case parser (dropWhile isSpace inp) of
    Success (r, inp') -> Success (f r, inp')
    Error err -> Error err

map' :: (Input -> Bool) -> String -> Parser a -> Input -> Result a
map' pred msg parser inp =
  case parser (dropWhile isSpace inp) of
    Success (r, inp') ->
      if pred inp'
      then Success r
      else Error (msg ++ show inp')
    Error err -> Error err


cut_varname :: Parser String
cut_varname [] = Error "Can't read symbol"
cut_varname s = Success (var, rest)
  where
    (x, inp') = span isAlpha s
    (y, inp'') = span (\x -> (isAlpha x) || (x == '_') || (isDigit x)) inp'
    var = x ++ y
    rest = inp''

