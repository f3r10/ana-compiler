module AnaCompiler (main) where

import System.Environment (getArgs)
import AnaCompiler.Parser
import AnaCompiler.Compile (compile, calcProgTyp, check, buildDefEnv)
import qualified Data.Text as T

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
           sexEps = stringToSexp (T.unpack (T.strip . T.pack $ content))
           prog = parseProgram sexEps 
           defEnv = buildDefEnv (fst prog)
           typ = calcProgTyp prog [] defEnv
           result = compile prog
           in
            result >>= putStrLn
        _ -> error "There is not present a program to compile"

