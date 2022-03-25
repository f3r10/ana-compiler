module AnaCompiler (main) where

import System.Environment (getArgs)
import AnaCompiler.Parser
import AnaCompiler.Compile (compile, calcProgTyp, check, buildDefEnv)

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
           sexEps = stringToSexp content
           prog = parseProgram sexEps 
           result = compile prog
           in
            result >>= putStrLn 
        _ ->
          let
            exp1 = "(def even (n : Num) : Bool (if (== n 0) true (odd (- n 1))))\n\
               \(def odd (n : Num) : Bool (if (== n 0) false (even (- n 1))))\n\
               \(even 2)"
            exp2 = "(+ -42 10)"
            sexEps = stringToSexp exp1
            prog = parseProgram sexEps
            defEnv = buildDefEnv (fst prog)
            typ = calcProgTyp prog [] defEnv
            checkRes = check prog
            result = compile prog
            in
              putStrLn $ show (fst prog)
            -- checkRes >>= print
            -- result >>= putStrLn

