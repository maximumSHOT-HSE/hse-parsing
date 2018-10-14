module Parser (parse) where -- only expose the top-level parsing function

import Combinators
import qualified Tokenizer as T
import Prelude hiding (lookup, (>>=), map, pred, return, elem)

data AST = ASum T.Operator AST AST
         | AProd T.Operator AST AST
         | AAssign String AST
         | ANum Integer
         | AIdent String
         | AUnaryMinus AST
         | APower T.Operator AST AST

-- TODO: Rewrite this without using Success and Error
parse :: String -> Maybe (Result AST)
parse input =
  case input of
    [] -> Nothing
    _ -> case expression input of
           Success (tree, ts') ->
             if null ts'
             then Just (Success tree)
             else Just (Error ("Syntax error on: " ++ show ts')) -- Only a prefix of the input is parsed
           Error err -> Just (Error err) -- Legitimate syntax error

expression :: Parser AST
expression =
  ( identifier >>= \(AIdent i) ->
    assignment |>
    expression >>= \e -> return (AAssign i e)
  )
  <|> ( term       >>= \l  -> -- Here the identifier is parsed twice :(
        plusMinus  >>= \op ->
        expression >>= \r  -> return (ASum op l r)
      )
  <|> term

term :: Parser AST
term =
  power >>= \l ->
  ( ( divMult >>= \op ->
      term    >>= \r  -> return (AProd op l r)
    )
    <|> return l
  )

power :: Parser AST
power =
  factor >>= \l ->
  ( ( power'' >>= \op ->
      power   >>= \r  -> return (APower op l r)
    )
    <|> return l
  )

factor :: Parser AST
factor =
  ( lparen |>
    expression >>= \e ->
    rparen |> return e -- No need to keep the parentheses
  )
  <|> identifier
  <|> number
  <|> unary_minus |> factor >>= \f -> return (AUnaryMinus f)

unary_minus :: Parser Char
unary_minus = char '-'

number :: Parser AST
number      = map (ANum   . T.value) (sat T.isValue (elem' T.isDigit))

identifier :: Parser AST
identifier = map (AIdent . T.varname) (sat T.isVarname (elem' T.isAlpha))

lparen :: Parser Char
lparen = char '('

rparen :: Parser Char
rparen = char ')'

assignment :: Parser Char
assignment = char '='

plusMinus :: Parser T.Operator
plusMinus = map T.operator (char '+' <|> char '-')

divMult :: Parser T.Operator
divMult   = map T.operator (char '/' <|> char '*')

power'' :: Parser T.Operator
power''     = map T.operator (char '^')

instance Show AST where
  show tree = "\n" ++ show' 0 tree
    where
      show' n t =
        (if n > 0 then \s -> concat (replicate (n - 1) "| ") ++ "|_" ++ s else id)
        (case t of
                  ASum  op l r  -> showOp op : "\n" ++ show' (ident n) l ++ "\n" ++ show' (ident n) r
                  AProd op l r  -> showOp op : "\n" ++ show' (ident n) l ++ "\n" ++ show' (ident n) r
                  AAssign  v e  -> v ++ " =\n" ++ show' (ident n) e
                  ANum   i      -> show i
                  AIdent i      -> show i
                  AUnaryMinus v -> showOp T.Minus : "\n" ++ show' (ident n) v
                  APower op l r -> showOp op : "\n" ++ show' (ident n) l ++ "\n" ++ show' (ident n) r
        )
      ident = (+1)
      showOp T.Plus  = '+'
      showOp T.Minus = '-'
      showOp T.Mult  = '*'
      showOp T.Div   = '/'
      showOp T.Power = '^'
