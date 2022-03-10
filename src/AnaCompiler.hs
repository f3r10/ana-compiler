module AnaCompiler (main) where

import System.Environment (getArgs)
import AnaCompiler.Parser
import AnaCompiler.Compile (compile, calcTyp, check)

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
           sexEp = stringToSexp content
           result = compile sexEp
           in
            result >>= putStrLn 
        _ ->
          let
            sexEp = stringToSexp "(let ((x (let ((x 5)) (sub1 x)))) (let ((y (sub1 x))) y))"
            sexpToExprRes = sexpToExpr sexEp
            typ = calcTyp sexpToExprRes []
            checkRes = check sexpToExprRes []
            result = compile sexEp
            in
            -- checkRes >>= print
            result >>= putStrLn
              -- putStrLn $ show sexpToExprRes
