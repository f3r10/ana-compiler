{-# LANGUAGE FlexibleContexts #-}

module AnaCompiler.Parser
  ( Sexp (..),
    stringToSexp,
    sexpToExpr,
    parseProgram,
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

parseProg :: Parser Sexp
parseProg = parseAtom2 <|> parseIntegers <|> parseVariablesNames <|> parseAtom <|> (sepBy parseList (many1 space) >>= return . List)

parseList :: Parser Sexp
parseList = between (char '(') (char ')') $ sepBy parseSexp (many1 space) >>= return . List

parseSexp :: Parser Sexp
parseSexp = parseAtom2 <|> parseIntegers <|> parseVariablesNames <|> parseAtom <|> parseList

parseIntegers :: Parser Sexp
parseIntegers = (((:) <$> char '-' <*> (option "" $ many1 digit)) <|> many1 digit) >>= return . Atom

parseVariablesNames :: Parser Sexp
parseVariablesNames = ((:) <$> oneOf (['a' .. 'z'] ++ ['A' .. 'Z']) <*> many (oneOf (['a' .. 'z'] ++ ['A' .. 'Z'] ++ ['0' .. '9']))) >>= return . Atom

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
      <|> string ":"
  )
    >>= return . Atom

parseAtom :: Parser Sexp
parseAtom =
  ( many1 operator
      <|> many1 timesOperator
  )
    >>= return . Atom

stringToSexp :: String -> [Sexp]
stringToSexp s = case parse parseProg "compiler" s of
  Left err -> error $ "Parse failed at String-> Sexp conversion: " ++ show err ++ " expr: " ++ s
  Right sexp ->
    case sexp of
      a@(Atom _) -> [a]
      List sexps -> sexps

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
    [Atom "set", Atom val, e1] -> ESet val (sexpToExpr e1)
    Atom "while" : condExp : listExpr ->
      let body =
            foldl
              ( \a b ->
                  case b of
                    simpleAtom@(Atom _) -> sexpToExpr simpleAtom : a
                    bExp@(List _) -> sexpToExpr bExp : a
              )
              []
              listExpr
       in EWhile (sexpToExpr condExp) (reverse body)
    [Atom "let", List ex1, simpleLetBody] ->
      let la =
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
      let la =
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
              (\a b -> sexpToExpr b : a)
              []
              listLetBody
       in ELet (reverse la) (reverse l2)
    [Atom nameFun, listParams] ->
       {- error $ show (sexpToExpr listParams) -} EApp nameFun [sexpToExpr listParams]
    [Atom s] -> stringToExpr s
    a -> error $ "Parse failed at Sexp->Expr conversion " ++ show a

parseDefParams :: [Sexp] -> TypEnv
parseDefParams sexps =
  case sexps of
    [] -> []
    Atom name : Atom ":" : Atom tye : rest ->
      case tye of
        "Num" -> (name, TNum) : parseDefParams rest
        "Bool" -> (name, TBool) : parseDefParams rest
        _ -> error "unvalid type param"
    _ -> error "unvalid def params"

parseDef :: Sexp -> Def
parseDef sexp =
  case sexp of
    Atom _ -> error "unvalid def exprs 1"
    List sexps ->
      case sexps of
        Atom "def" : Atom name : List paramsList : Atom ":" : Atom returnType : rest ->
          let l2 =
                foldl
                  (\a b -> sexpToExpr b : a)
                  []
                  rest
           in {- error $ show (l2) -}
              case returnType of
                "Num" -> DFun name (parseDefParams paramsList) TNum (reverse l2)
                "Bool" -> DFun name (parseDefParams paramsList) TBool (reverse l2)
                _ -> error "unvalid def return type"
        a -> error $ "unvalid def exprs 2: " ++ show a

parseProgram :: [Sexp] -> Prog
parseProgram sexps =
  case sexps of
    [] -> error "Invalid: Empty program"
    [e] -> ([], sexpToExpr e)
    e : es ->
      let parseE = parseDef e
          (defs, main) = parseProgram es
       in (parseE : defs, main)
