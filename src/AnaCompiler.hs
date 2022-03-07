module AnaCompiler (main) where

import System.Environment (getArgs)
import AnaCompiler.Parser
import AnaCompiler.Compile (compile)

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
            sexEp = stringToSexp "(let ((x 5)) (add1 x))" --"(let ((x 5) (y 6)) ((add1 y) (add1 x) (+ x y)))"
            sexpToExprRes = sexpToExpr sexEp
            result = compile sexEp
            in
            -- result >>= putStrLn
              putStrLn $ show sexEp
