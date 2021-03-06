module AnaCompiler.Expr (Expr (..), Prim2 (..), Prim1 (..), Typ (..), Def (..), Prog, TypEnv, TEnv, DefTypEnv, TypAlias (..), TypAliasEnv) where

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
  | IsNull
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
  | EVecSet String Int Expr
  | EIf Expr Expr Expr
  | EPrim1 Prim1 Expr
  | EPrim2 Prim2 Expr Expr
  | EVector [Expr]
  | EDict [(String, Expr)]
  | EDictGet String String
  | EDictSet String String Expr
  | EGet String Int
  | EBool Bool
  | ETuple Expr Expr Typ
  | EHead String
  | ETail String
  | ENil Typ
  | EApp String [Expr]
  deriving (Show)

type VariableName = String

type VariablePosition = Int

type TypEnv = [(VariableName, Typ)]

type TEnv = [(VariableName, VariablePosition)]

type DefTypEnv = [(String, (Typ, TypEnv))]

type TypAliasEnv = [(String, Typ)]

data Typ
  = TNum
  | TBool
  | TTuple Typ
  | TName String
  | TVec Typ
  | TDict [(String, Typ)]
  deriving (Eq, Show)

data Def
  = DFun String [(String, Typ)] Typ [Expr]
  deriving (Show)

data TypAlias
  = TypAlias String Typ
  deriving (Show)

{- instance Show Def where
  show (DFun name args typ body) = "DFun " ++ name -}

type Prog = ([Def], [TypAlias], Expr)
