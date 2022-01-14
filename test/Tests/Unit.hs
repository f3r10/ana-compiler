{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Tests.Unit (main, spec) where

import AnaCompiler.Compile (compile)
import AnaCompiler.Parser (Sexp (Atom, List))
import qualified AnaCompiler.Parser as Parser
import qualified Data.Text as T
import System.Directory (removeFile)
import System.Process (callCommand, callProcess, createProcess, proc, readProcess)
import Test.Hspec
import Test.QuickCheck
import Test.QuickCheck.Exception (evaluate)
import Text.Printf (printf)

type Program = String

test_run :: Program -> String -> IO T.Text
test_run program name =
  let sexp = Parser.stringToSexp program
      result = compile sexp
      name_s = printf "output/%s.s" name
      name_o = printf "output/%s.o" name
      name_run = printf "output/%s.run" name
      writeResult = writeFile name_s result
      nasm_cmd = readProcess "nasm" ["-f elf64", "-o " ++ name_o, name_s] []
      clang_cmd = callCommand ("clang " ++ "-o " ++ name_run ++ " main.c " ++ name_o)
      result_o = readProcess name_run [] []
   in do
        _ <- writeResult
        _ <- nasm_cmd
        _ <- clang_cmd
        r <- T.strip . T.pack <$> result_o
        _ <- removeFile name_s
        _ <- removeFile name_o
        _ <- removeFile name_run
        pure r

-- `main` is here so that this module can be run from GHCi on its own.  It is
-- not needed for automatic spec discovery.
main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "compiler" $ do
    it "forty_one" $ do
      a <- test_run "(sub1 42)" "forty_one"
      shouldBe a "41"
    it "forty" $ do
      a <- test_run "(sub1 (sub1 42))" "forty"
      shouldBe a "40"
    it "add1" $ do
      a <- test_run "(add1 (add1 (add1 3)))" "add1"
      shouldBe a "6"
    it "def_x" $ do
      a <- test_run "(let ((x 5)) x)" "def_x"
      shouldBe a "5"
    it "def_x2" $ do
      a <- test_run "(let ((x 5)) (sub1 x))" "def_x2"
      shouldBe a "4"
    it "def_x3" $ do
      a <- test_run "(let ((x 5)) (let ((x 67)) (sub1 x)))" "def_x3"
      shouldBe a "66"
    it "def_x4" $ do
      a <- test_run "(let ((x (let ((x 5)) (sub1 x)))) (sub1 x))" "def_x4"
      shouldBe a "3"
    it "def_x5" $ do
      a <- test_run "(let ((x 10) (y 7)) (+ x (sub1 y)))" "def_x5"
      shouldBe a "16"
    it "addnums" $ do
      a <- test_run "(+ 5 10)" "addnums"
      shouldBe a "15"
    it "nested_add" $ do
      a <- test_run "(+ 5 (+ 10 20))" "nested_add"
      shouldBe a "35"
    it "nested_add_2" $ do
      a <- test_run "(+ (- 10 5) 20)" "nested_add_2"
      shouldBe a "25"
    it "failLet" $
      let sexp = Parser.stringToSexp "(let ((x  1) (y 1) (x 10)) x)"
          result = compile sexp
       in evaluate result `shouldThrow` errorCall "Compile error: Duplicate binding"
    it "failID" $
      let sexp = Parser.stringToSexp "x"
          result = compile sexp
       in evaluate result `shouldThrow` errorCall "Compile error: Unbound variable identifier x"
    it "property" $
      property $
        \() -> () === ()
  describe "parser expression" $ do
    it "parse simple number" $ do
      shouldBe (Parser.stringToSexp "(2)") (List [Atom "2"])
    it "parse an nested let expresion with an add operation" $ do
      shouldBe (Parser.stringToSexp "(let (x 10) (let (y 10) (+ x y)))") (List [Atom "let", List [Atom "x", Atom "10"], List [Atom "let", List [Atom "y", Atom "10"], List [Atom "+", Atom "x", Atom "y"]]])
