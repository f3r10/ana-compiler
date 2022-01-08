{-# LANGUAGE FlexibleContexts #-}
module Main where

import Data.Char (isDigit, isNumber, isSymbol)
import Text.ParserCombinators.Parsec
import Text.Printf
import System.Environment (getArgs)
import Text.Parsec


data Sexp
  = Atom String
  | List [Sexp]
  deriving (Show)

parseSexp :: Parser Sexp
parseSexp = parseAtom <|> parseList

operator :: (Stream s m Char) => ParsecT s u m Char
operator = satisfy isSymbol

parseAtom :: Parser Sexp
parseAtom = ((many1 letter) <|> (many1 digit) <|> (many1 operator)) >>= return . Atom

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
    [Atom "+", e1, e2] -> EPlus (sexpToExpr e1) (sexpToExpr e2)
    [Atom "let", List [Atom name, e1], e2] ->
      ELet name (sexpToExpr e1) (sexpToExpr e2)
    a -> error $ "Parse failed at Sexp->Expr conversion " ++ show a

-- ELet "x" (ENum 10) (EOp Inc (EId "x"))
stackloc :: Int -> Int
stackloc i = i * 8

stackval :: Int -> String
stackval i = "[rsp - " ++ show (stackloc i) ++ "]"

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
    EPlus e1 e2 -> 
      let
        e1is = exprToInstrs e1 si env
        e2is = exprToInstrs e2 (si + 1) env
        in
        e1is ++ ["mov " ++ stackval si ++ ", rax"] 
        ++ e2is ++ [ "mov " ++ stackval (si + 1) ++ ", rax"] 
        ++ ["mov rax, " ++ stackval si
           , "add rax, " ++ stackval (si + 1)]
    ELet x value body ->
      let v_is = exprToInstrs value si env
          new_env = (x, si) : env
          store = "mov [rsp - " ++ show (stackloc si) ++ "], rax"
          b_is = exprToInstrs body (si + 1) new_env
       in v_is ++ [store] ++ b_is

compile :: String -> String
compile s =
  let header = "section .text\nglobal our_code_starts_here\nour_code_starts_here:\n"
      sexEp = stringToSexp s
      expr = sexpToExpr sexEp
      body = concatMap (printf " %s\n") $ exprToInstrs expr 1 []
      footer = " ret\n"
   in body `seq` header ++ body ++ footer

main :: IO ()
main =
  {- let 
   a = "(let (x 10) (let (y (inc x)) (let (z (inc y)) z)))"
   b = "(let (x (let (y 10) (inc y))) (let (z (inc x)) z))"
   in {- sexEp = stringToSexp a
      expr = sexpToExpr sexEp
      in
      print expr -}
      putStrLn $ compile b -}
  getArgs >>=
    \args ->
      case args of
        [input] -> do
          content <- readFile input 
          let
           result = compile content
           in
            putStrLn result
        _ -> 
          putStrLn "Usage: compile name.int"
