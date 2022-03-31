module AnaCompiler.Expr (Expr(..), Prim2(..), Prim1(..), Typ(..), Def(..), Prog, TypEnv, TEnv, DefTypEnv) where

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
  | Print
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
  | EWhile Expr [Expr]
  | ESet String Expr
  | EIf Expr Expr Expr
  | EPrim1 Prim1 Expr
  | EPrim2 Prim2 Expr Expr
  | EBool Bool
  | EApp String [Expr]
  deriving (Show)


type VariableName = String
type VariablePosition = Int

type TypEnv = [(VariableName, Typ)]
type TEnv = [(VariableName, VariablePosition)]
type DefTypEnv = [(String, (Typ, TypEnv))]

data Typ
  = TNum
  | TBool
  deriving (Eq, Show)

data Def = 
  DFun String [(String, Typ)] Typ [Expr]
  deriving (Show)


{- instance Show Def where
  show (DFun name args typ body) = "DFun " ++ name -}

type Prog = ([Def], Expr)
