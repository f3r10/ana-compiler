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
