module Main where

import Data.Char (isDigit, isNumber)
import System.Environment
import Text.ParserCombinators.Parsec
import Text.Printf

data Sexp
  = Atom String
  | List [Sexp]
  deriving (Show)

parseSexp :: Parser Sexp
parseSexp = parseAtom <|> parseList

parseAtom :: Parser Sexp
parseAtom = ((many1 letter) <|> (many1 digit)) >>= return . Atom

parseList :: Parser Sexp
parseList = between (char '(') (char ')') $ sepBy parseSexp (many1 space) >>= return . List

stringToSexp :: String -> Sexp
stringToSexp s = case parse parseSexp "compiler" s of
  Left err -> error $ "Parse failed at String->Sexp conversion: " ++ show err
  Right sexp -> sexp

data Op
  = Inc
  | Dec
  deriving (Show)

data Expr
  = ENum Int
  | EOp Op Expr
  | ELet String Expr Expr
  | EId String
  deriving (Show)

{- List [
  Atom "let",List [Atom "x",Atom "10"],List [
    Atom "let",List [Atom "y",List [ Atom "inc",Atom "x"]]]] -}
sexpToExpr :: Sexp -> Expr
sexpToExpr (Atom s) =
  if all isDigit s
    then ENum (read s)
    else EId s
sexpToExpr (List sexps) =
  case sexps of
    [Atom "inc", arg] -> EOp Inc (sexpToExpr arg)
    [Atom "dec", arg] -> EOp Dec (sexpToExpr arg)
    [Atom "let", List [Atom name, e1], e2] ->
      ELet name (sexpToExpr e1) (sexpToExpr e2)
    a -> error $ "Parse failed at Sexp->Expr conversion " ++ show a

-- ELet "x" (ENum 10) (EOp Inc (EId "x"))
stackloc :: Int -> Int
stackloc i = i * 8

type TEnv = [(String, Int)]

find :: TEnv -> String -> Maybe Int
find env x =
  case env of
    [] -> Nothing
    (y, i) : rest ->
      if y == x
        then Just i
        else find rest x

-- si -> stack index
exprToInstrs :: Expr -> Int -> TEnv -> [String]
exprToInstrs expr si env =
  case expr of
    EId x ->
      case find env x of
        Nothing -> error "Unbound id"
        Just i -> ["mov rax, [rsp - " ++ show (stackloc i) ++ "]"]
    ENum n -> ["mov rax, " ++ show n]
    EOp Inc e -> exprToInstrs e si env ++ ["add rax, 1"]
    EOp Dec e -> exprToInstrs e si env ++ ["sub rax, 1"]
    ELet x value body ->
      let v_is = exprToInstrs value si env
          new_env = (x, si) : env
          store = "mov [rsp - " ++ show (stackloc si) ++ "], rax"
          b_is = exprToInstrs body (si + 1) new_env
       in v_is ++ [store] ++ b_is

-- _ -> error "Parse failed at Expr -> String conversion "

compile :: String -> String
compile s =
  let header = "section . text\nglobal our_code_starts_here\nour_code_starts_here:\n"
      sexEp = stringToSexp s
      expr = sexpToExpr sexEp
      body = concatMap (printf " %s\n") $ exprToInstrs expr 1 []
      footer = " ret\n"
   in body `seq` header ++ body ++ footer

main :: IO ()
main =
  let 
   a = "(let (x 10) (let (y (inc x)) (let (z (inc y)) z)))"
   b = "(let (x (let (y 10) (inc y))) (let (z (inc x)) z))"
   in {- sexEp = stringToSexp a
      expr = sexpToExpr sexEp
      in
      print expr -}
      putStrLn $ compile b

-- case a of
--   Left e -> print "error"
--   Right sexps -> print $ sexpToExpr sexps
