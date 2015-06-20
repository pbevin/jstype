module Builtins.NumberSpec where

import Test.Hspec
import Expectations
import Runtime
import Eval

spec :: Spec
spec = do
  describe "Number() function" $ do
    it "returns a primitive number when given one" $ do
      jsEvalExpr "Number(42)" `shouldReturn` VNum 42

    it "converts other arguments using toNumber" $ do
      jsEvalExpr "Number(false)"  `shouldReturn` VNum 0
      jsEvalExpr "Number(true)"   `shouldReturn` VNum 1
      jsEvalExpr "Number('96.8')" `shouldReturn` VNum 96.8
      jsEvalExpr "isNaN(Number('aaa'))" `shouldReturn` VBool True
      jsEvalExpr "Number('Infinity')" `shouldReturn` VNum (1/0)

    it "has no enumerable properties" $
      -- test case: 8.6.1_A2
      runJStr "for (n in Number) { console.log(n); }" `shouldReturn` Right ""

  describe "Number.prototype.toString()" $ do
    it "prints an integer" $ do
      jsEvalExpr "(42).toString()" `shouldReturn` VStr "42"

    it "prints a float" $ do
      jsEvalExpr "(42.8).toString()" `shouldReturn` VStr "42.8"

    it "prints a boxed integer" $ do
      jsEvalExpr "new Number(42).toString()" `shouldReturn` VStr "42"

    it "prints a boxed float" $ do
      jsEvalExpr "new Number(42.8).toString()" `shouldReturn` VStr "42.8"
