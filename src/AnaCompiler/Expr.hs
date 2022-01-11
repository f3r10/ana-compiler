module AnaCompiler.Expr (Expr(..), Prim2(..)) where

{- (*
expr := <number>
     | (let (<name> <expr>) <expr>) 
     | (+ <expr> <expr>)
     | <name>
  *) -}
data Prim2 
  = Plus
  | Minus
  deriving (Show)

data Expr
  = ENum Int
  | EId String
  | ELet String Expr Expr
  | EPrim2 Prim2 Expr Expr
  deriving (Show)
