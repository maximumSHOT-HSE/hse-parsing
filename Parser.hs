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
         | ACmd T.Operator AST AST
         | AComma T.Operator AST AST
         | AList AST
         | AEPS
         | AConcat T.Operator AST AST

parse :: String -> Maybe (Result AST)
parse input =
  case input of
    [] -> Nothing
    _  -> Just (finish commands input)

commands :: Parser AST
commands =
  expression >>= \l ->
  ( ( cmd       >>= \op ->
      commands  >>= \r  -> return (ACmd op l r)
    )
    <|> return l
  )

expression :: Parser AST
expression =
  ( identifier >>= \(AIdent i) ->
    assignment |>
    expression >>= \r -> return (AAssign i r)
  )
  <|>
  ( identifier >>= \i  ->
    concat_op  >>= \op ->
    list_expr  >>= \e  -> return (AConcat op i e)
  )
  <|>
  (arithmetic >>= \x -> return x)
  <|>
  (list_expr  >>= \x -> return x)

list_expr :: Parser AST
list_expr = 
  ( identifier >>= \(AIdent i) ->
    ( assignment |>
      list_expr >>= \e -> return (AAssign i e)
    )
    <|>
    ( concat_op >>= \op ->
      list_expr >>= \r  -> 
      return (AConcat op (AIdent i) r)
    )
    <|> return (AIdent i)
  )
  <|>
  ( list >>= \l -> 
    ( concat_op >>= \op ->
      list_expr >>= \r  -> return (AConcat op l r)
    )
    <|> return l
  )

arithmetic :: Parser AST
arithmetic =
  ( identifier >>= \(AIdent i) ->
    assignment |>
    arithmetic >>= \e -> return (AAssign i e)
  )
  <|> 
  term       >>= \l  -> -- Here the identifier is parsed twice :(
  ( ( plusMinus  >>= \op ->
        arithmetic >>= \r  -> return (ASum op l r)
    )
    <|>
    return l
  )

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
    arithmetic >>= \e ->
    rparen |> return e -- No need to keep the parentheses
  )
  <|> identifier
  <|> number
  <|> (unary_minus |> factor >>= \f -> return (AUnaryMinus f))

list :: Parser AST
list = 
  ( sq_lparen |>
    (nodeSequence <|> epsilon) >>= \e ->
    sq_rparen |> return (AList e)
  )

epsilon :: Parser AST
epsilon = return AEPS

nodeSequence :: Parser AST
nodeSequence =
  expression >>= \l ->
  ( comma        >>= \op ->
    nodeSequence >>= \r  ->
    return (AComma op l r)
  )
  <|> return l

unary_minus :: Parser Char
unary_minus = char '-'

number :: Parser AST
number      = map (ANum   . T.value) (sat T.isValue (elem' T.isDigit))

identifier :: Parser AST
identifier = map (AIdent . T.varname) (sat T.isVarname cut_varname)

lparen :: Parser Char
lparen = char '('

rparen :: Parser Char
rparen = char ')'

sq_lparen :: Parser Char
sq_lparen = char '['

sq_rparen :: Parser Char
sq_rparen = char ']'

assignment :: Parser Char
assignment = char '='

plusMinus :: Parser T.Operator
plusMinus = map T.operator (char '+' <|> char '-')

comma :: Parser T.Operator
comma = map T.operator (char ',')

divMult :: Parser T.Operator
divMult   = map T.operator (char '/' <|> char '*')

power'' :: Parser T.Operator
power''     = map T.operator (char '^')

cmd :: Parser T.Operator
cmd         = map T.operator (char ';')

concat_op :: Parser T.Operator
concat_op   = map T.concat_op (sat T.isConcatOp elem2)

instance Show AST where
  show tree = "\n" ++ show' 0 tree
    where
      show' n t =
        (if n > 0 then \s -> concat (replicate (n - 1) "| ") ++ "|_" ++ s else id)
        (case t of
                  ASum  op l r   -> showOp op : "\n" ++ show' (ident n) l ++ "\n" ++ show' (ident n) r
                  AProd op l r   -> showOp op : "\n" ++ show' (ident n) l ++ "\n" ++ show' (ident n) r
                  AAssign  v e   -> v ++ " =\n" ++ show' (ident n) e
                  ANum   i       -> show i
                  AIdent i       -> show i
                  AUnaryMinus v  -> showOp T.Minus : "\n" ++ show' (ident n) v
                  APower op l r  -> showOp op : "\n" ++ show' (ident n) l ++ "\n" ++ show' (ident n) r
                  ACmd op l r    -> showOp op : "\n" ++ show' (ident n) l ++ "\n" ++ show' (ident n) r
                  AComma op l r  -> showOp op : "\n" ++ show' (ident n) l ++ "\n" ++ show' (ident n) r
                  AList v        -> 'L' : "\n" ++ show' (ident n) v
                  AEPS           -> "epsilon"
                  AConcat op l r -> "++\n" ++ show' (ident n) l ++ "\n" ++ show' (ident n) r
        )
      ident = (+1)
      showOp T.Plus  = '+'
      showOp T.Minus = '-'
      showOp T.Mult  = '*'
      showOp T.Div   = '/'
      showOp T.Power = '^'
      showOp T.Cmd   = ';'
      showOp T.Comma = ','
