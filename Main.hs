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
