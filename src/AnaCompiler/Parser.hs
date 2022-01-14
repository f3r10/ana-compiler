{-# LANGUAGE FlexibleContexts #-}

module AnaCompiler.Parser
  ( Sexp (..),
    stringToSexp,
    sexpToExpr,
  )
where

import Data.Char (isSymbol, isDigit)
import Text.Parsec
import Text.ParserCombinators.Parsec
import AnaCompiler.Expr

data Sexp
  = Atom String
  | List [Sexp]
  deriving (Show, Eq)

parseSexp :: Parser Sexp
parseSexp = parseAtom2 <|>  parseAtom <|> parseList 

operator :: (Stream s m Char) => ParsecT s u m Char
operator = satisfy isSymbol

minusOperator :: (Stream s m Char) => ParsecT s u m Char
minusOperator = satisfy (== '-')

-- this parsers has to go first since it has to detect an string ending up with a digit
parseAtom2 :: Parser Sexp
parseAtom2 = (string "add1" <|> string "sub1") >>= return . Atom

parseAtom :: Parser Sexp
parseAtom = ( many1 letter 
          <|> many1 digit 
          <|> many1 operator 
          <|> many1 minusOperator 
            ) 
          >>= return . Atom

parseList :: Parser Sexp
parseList = between (char '(') (char ')') $ sepBy parseSexp (many1 space) >>= return . List

stringToSexp :: String -> Sexp
stringToSexp s = case parse parseSexp "compiler" s of
  Left err -> error $ "Parse failed at String-> Sexp conversion: " ++ show err ++ " expr: " ++ s
  Right sexp -> sexp

-- (let ((x 5)) (add1 x)) -> 
  -- List [Atom "let",List [List [Atom "x",Atom "5"]],List [Atom "add1",Atom "x"]]
  -- Let ([("x", Number(5))], Prim1(Add1, Id("x")))

--  (let ((x (+ 5 (+ 10 20)))) (+ x x)) ->
  -- List [Atom "let",List [List [Atom "x",List [Atom "+",Atom "5",List [Atom "+",Atom "10",Atom "20"]]]],List [Atom "+",Atom "x",Atom "x"]]

-- (let ((x 10) (y 7)) x) ->
  -- List [Atom "let",List [List [Atom "x",Atom "10"],List [Atom "y",Atom "7"]],Atom "x"]
sexpToExpr :: Sexp -> Expr
sexpToExpr (Atom s) =
  if all isDigit s
    then ENum (read s)
    else EId s
sexpToExpr (List sexps) =
  case sexps of
    [Atom "+", e1, e2] -> EPrim2 Plus (sexpToExpr e1) (sexpToExpr e2)
    [Atom "-", e1, e2] -> EPrim2 Minus (sexpToExpr e1) (sexpToExpr e2)
    [Atom "add1", e1] -> EPrim1 Add1 (sexpToExpr e1)
    [Atom "sub1", e1] -> EPrim1 Sub1 (sexpToExpr e1)
    [Atom "let", List ex1, simple_e_2] ->
      let 
        la = foldl (\a b-> 
          case b of
            List [Atom n, ie] -> (n, sexpToExpr ie) : a
            _ -> error $ "invalid let rec expression " ++ show b ) [] ex1 
      in ELet (reverse la) (sexpToExpr simple_e_2)
    a -> error $ "Parse failed at Sexp->Expr conversion " ++ show a
