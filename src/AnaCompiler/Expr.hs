module AnaCompiler.Expr (Expr(..), Prim2(..), Prim1(..)) where

{- (*
expr := <number>
     | (let (<name> <expr>) <expr>) 
     | (+ <expr> <expr>)
     | <name>
  *) -}
data Prim1
  = Add1
  | Sub1
  | IsNum
  | IsBool
  deriving (Show)

data Prim2 
  = Plus
  | Minus
  | Times
  | Less
  | Greater
  | Equal
  deriving (Show)

data Expr
  = ENum Int
  | EId String
  | ELet [(String, Expr)] [Expr]
  | ESet String Expr
  | EIf Expr Expr Expr
  | EPrim1 Prim1 Expr
  | EPrim2 Prim2 Expr Expr
  | EBool Bool
  deriving (Show)
