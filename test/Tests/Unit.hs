{-# LANGUAGE ScopedTypeVariables #-}
module Tests.Unit (main, spec) where

import Test.Hspec
import Test.QuickCheck
import qualified AnaCompiler.Parser as Parser
import AnaCompiler.Parser (Sexp(Atom, List))

-- `main` is here so that this module can be run from GHCi on its own.  It is
-- not needed for automatic spec discovery.
main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "example" $ do
    it "test" $ do
      shouldBe () ()
    it "property" $ property $
      \() -> () === ()
  describe "parser expresion" $ do
    it "parse simple number" $ do
      shouldBe (Parser.stringToSexp "(2)") (List [Atom "2"])
    it "parse an nested let expresion with an add operation" $ do
      shouldBe ( Parser.stringToSexp "(let (x 10) (let (y 10) (+ x y)))" ) (List [Atom "let", List [Atom "x", Atom "10"], List [Atom "let", List [Atom "y", Atom "10"], List [Atom "+", Atom "x", Atom "y"]]])
