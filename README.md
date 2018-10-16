# hse-parsing
A simple recursive descent parser. Written for the formal languages course in HSE. The language is described with the following grammar:

```
S -> Commands | \epsilon

Commands -> Expr (; Commands)?

Expr > Ident = Expr
  | Arithmetic
  | ListExpr

ListExpr -> Ident = ListExpr
      | Ident (++ ListExpr)?
      | List  (++ ListExpr)?

Arithmetic -> Ident = Arithmetic
      | Term ((+ | -) Arithmetic)?

Term -> Power ((* | /) Term)?

Power -> Factor (^ Power)?

Factor -> Ident 
        | Num 
        | '(' Arithmetic ')'
        | -Factor

List = '[' (NodeSequence | epsilon) ']'

NodeSequence = (Arithmetic | List) (, List)?

Ident -> [a-z]+

Num -> [0-9]+
```

Running the build script `build.sh` generates an executable `Main`. `Main` parses several inputs specified and terminates.

To run parser on your input, load `Main` into the interpreter `ghci` and execute `parse <input>` or modify `Main.hs`.

This should work on any version of the haskell compiler, but has only been tested on `ghc 8.4.2`.
