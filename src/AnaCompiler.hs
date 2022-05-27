module AnaCompiler (main) where

import System.Environment (getArgs)
import AnaCompiler.Parser
import AnaCompiler.Compile (compile, calcProgTyp, buildDefEnv, buildTypAliasEnv)
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
           prog@(defs, typs, main) = parseProgram sexEps 
           defEnv = buildDefEnv defs
           typAliasEnv = buildTypAliasEnv typs
           typ = calcProgTyp prog [] defEnv typAliasEnv
           result = compile prog
           in
            result >>= putStrLn
            -- typ >>= print
            -- print prog
        _ -> error "There is not present a program to compile"

