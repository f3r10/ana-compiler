{-# LANGUAGE FlexibleContexts #-}

module AnaCompiler.Parser
  ( Sexp (..),
    stringToSexp,
    sexpToExpr,
  )
where

import AnaCompiler.Expr
import Data.Char (isDigit, isSymbol)
import Text.Parsec
import Text.ParserCombinators.Parsec

data Sexp
  = Atom String
  | List [Sexp]
  deriving (Show, Eq)

parseSexp :: Parser Sexp
parseSexp = parseAtom2 <|> parseIntegers <|> parseVariablesNames <|> parseAtom <|> parseList

parseIntegers :: Parser Sexp
parseIntegers = (((:) <$> char '-' <*> (option "" $ many1 digit)) <|> many1 digit) >>= return . Atom

parseVariablesNames :: Parser Sexp
parseVariablesNames = ((:) <$> oneOf (['a'..'z'] ++ ['A'..'Z']) <*> many (oneOf (['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9']))) >>= return . Atom

operator :: (Stream s m Char) => ParsecT s u m Char
operator = satisfy isSymbol

timesOperator :: (Stream s m Char) => ParsecT s u m Char
timesOperator = satisfy (== '*')

-- this parsers has to go first since it has to detect an string ending up with a digit
parseAtom2 :: Parser Sexp
parseAtom2 =
  ( string "add1"
      -- <|> string "sub1"
      <|> string "true"
      <|> string "false"
      <|> string "=="
  )
    >>= return . Atom

parseAtom :: Parser Sexp
parseAtom =
  ( many1 operator
      <|> many1 timesOperator
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
anaMax :: Integer
anaMax = round (2.0 ** 62.0) - 1

anaMin :: Integer
anaMin = - round (2.0 ** 62.0)

checkNumberLimits :: String -> Bool
checkNumberLimits s =
  let num = read s
   in anaMin < num && num < anaMax

converToNumber :: String -> Expr
converToNumber s =
  if checkNumberLimits s
    then ENum (read s)
    else error $ "Compile error: Non-representable number " ++ s

stringToExpr :: String -> Expr
stringToExpr s =
  case s of
    "true" -> EBool True
    "false" -> EBool False
    '-' : rest ->
      if all isDigit rest
        then converToNumber s
        else error $ "Compile error: Invalid number expression " ++ show s
    _ ->
      if all isDigit s
        then converToNumber s
        else EId s

sexpToExpr :: Sexp -> Expr
sexpToExpr (Atom s) = stringToExpr s

sexpToExpr (List sexps) =
  case sexps of
    [Atom "+", e1, e2] -> EPrim2 Plus (sexpToExpr e1) (sexpToExpr e2)
    [Atom "-", e1, e2] -> EPrim2 Minus (sexpToExpr e1) (sexpToExpr e2)
    [Atom "*", e1, e2] -> EPrim2 Times (sexpToExpr e1) (sexpToExpr e2)
    [Atom "==", e1, e2] -> EPrim2 Equal (sexpToExpr e1) (sexpToExpr e2)
    [Atom "<", e1, e2] -> EPrim2 Less (sexpToExpr e1) (sexpToExpr e2)
    [Atom ">", e1, e2] -> EPrim2 Greater (sexpToExpr e1) (sexpToExpr e2)
    [Atom "add1", e1] -> EPrim1 Add1 (sexpToExpr e1)
    [Atom "sub1", e1] -> EPrim1 Sub1 (sexpToExpr e1)
    [Atom "isNum", e1] -> EPrim1 IsNum (sexpToExpr e1)
    [Atom "isBool", e1] -> EPrim1 IsBool (sexpToExpr e1)
    [Atom "true"] -> EBool True
    [Atom "false"] -> EBool False
    [Atom "if", e1, e2, e3] -> EIf (sexpToExpr e1) (sexpToExpr e2) (sexpToExpr e3)
    [Atom "set", Atom val, e1 ] -> ESet val (sexpToExpr e1)
    [Atom "let", List ex1, simpleLetBody] ->
      let 
        la =
            foldl
              ( \a b ->
                  case b of
                    List [Atom n, ie] -> (n, sexpToExpr ie) : a
                    _ -> error $ "invalid let rec expression " ++ show b
              )
              []
              ex1
       in ELet (reverse la) [sexpToExpr simpleLetBody]
    Atom "let" : List ex1 : listLetBody ->
      let 
        la =
            foldl
              ( \a b ->
                  case b of
                    List [Atom n, ie] -> (n, sexpToExpr ie) : a
                    _ -> error $ "invalid let rec expression " ++ show b
              )
              []
              ex1
        l2 = 
            foldl
              ( \a b ->
                case b of
                  simpleAtom@(Atom _) -> sexpToExpr simpleAtom : a
                  bExp@(List _) -> sexpToExpr bExp : a) [] listLetBody
       in ELet (reverse la) (reverse l2)
    a -> error $ "Parse failed at Sexp->Expr conversion " ++ show a
