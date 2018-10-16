{-# LANGUAGE FlexibleInstances #-}

module Main where

import Parser
import Combinators (Result (Success, Error))

runParser :: String -> IO ()
runParser input = do
  putStrLn input
  print $ parse input
  putStrLn ""

instance {-# OVERLAPPING #-} Show a => Show (Maybe (Result a)) where
  show (Just (Success tree)) = show tree
  show (Just (Error err)) = "Syntax error: " ++ err
  show Nothing = "Empty tree"

main :: IO ()
main = do
  runParser "1-2-3"
  runParser "((  (9)  ))    "
  runParser "1*   2 -     3  /4+  5         "
  runParser "1"
  runParser "1 + 2"
  runParser "1 +5  "
  runParser "123+56*21/78"
  runParser "   122132     *     2123213     - 321321 /    21324     +     52411        "
  runParser "   var = 32 *3 + (   83)/32 "
  runParser "awadwjfwek = 42"
  runParser "var = 32*3 + (- 83)/32"
  runParser " -   ---         5 - 6   "
  runParser " var = 32^3 + (- 83)/32       "
  runParser "a*b^c/d+e-f^(-2)*d"
  runParser "x = 13; y = z = 42 + 6; 777"
  runParser " var = 32^3 + (- 83)/32; a = 43; 44    "

  runParser "[1,2,3] ; [[z]]"

  runParser "   [ b = 13, [z],  42 + 6] ; [31, 25]; 72817; var = 5^-2"
  runParser "[1, 2, [2 + 3, val = 123^15 + 10, [a], [zero, one, two, 3, 4, 5 + 0, val = 6]]]"
  runParser "[1, 2, [2 + 3, val = 123^15 + 10, [a], [zero, one, two, 3, 4, 5 + 0, val = 6]], lol]; same; 777"

  runParser "abc1"
  runParser "abc_a"
  runParser "_var"

  runParser "[1, 2, [2 + 3, val = 123^15 + 10, [a], [zero, one, two, 3, 4, 5 + 0, val_1 = 6]], lol]; same; 777"

  runParser "1abc"
  runParser "abc+"

  runParser "[[1], []]"

  runParser "a = [1,2, 3]; b = 56^-2"

  runParser "a = [1,2,3] ++ [4,5,6]"

  runParser "[1 + 2, 3 + 4, [7, 8, 9]] ++ [a]; x = []; [1, 2, 3]"
  runParser "[1-2-3] ++ x ++ [1, [12+1]]"
  runParser "[123] ++ [456]"
  runParser "[1-2-3] ++ x ++ [1, [12+1]]"
  runParser "x = y ++ z"

  runParser "x^y"

  runParser "[1] ++ [2] ++ [3, 4, var = 5, [abc] ++ a ]"
  runParser "[1] ++ [2] ++ [3, 4, var = 5, [abc] ++ [ab]]"

  runParser "a * b / c * d"

  runParser "var^43 + 32^34^(-12*34^21) - 3^-1"

  runParser "x^y"
  