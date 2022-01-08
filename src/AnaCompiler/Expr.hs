module AnaCompiler.Expr (Expr(..)) where

{- (*
expr := <number>
     | (let (<name> <expr>) <expr>) 
     | (+ <expr> <expr>)
     | <name>
  *) -}

data Expr
  = ENum Int
  | EId String
  | ELet String Expr Expr
  | EPlus Expr Expr
  deriving (Show)
