{-# LANGUAGE OverloadedStrings #-}

module Builtins.NumberSpec where

import Test.Hspec
import Expectations
import Runtime
import Eval

spec :: Spec
spec = do
  describe "Number() function" $ do
    it "returns a primitive number when given one" $ do
      jsEvalExpr "Number(42)" `shouldReturn` VInt 42

    describe "converts other arguments using toNumber" $ do
      specify "false" $ jsEvalExpr "Number(false)"  `shouldReturn` VInt 0
      specify "true" $ jsEvalExpr "Number(true)"   `shouldReturn` VInt 1
      specify "floating point" $ jsEvalExpr "Number('96.8')" `shouldReturn` VNum 96.8
      specify "non-number" $ jsEvalExpr "isNaN(Number('aaa'))" `shouldReturn` VBool True
      specify "Infinity" $ jsEvalExpr "Number('Infinity')" `shouldReturn` VNum (1/0)

    -- it "allows a decimal starting with 00." $ do
    --   jsEvalExpr("Number('00.123')") `shouldReturn` VNum 0.123

    it "has prototype Function.prototype" $ do
      jsEvalExpr "Function.prototype.isPrototypeOf(Number)" `shouldReturn` VBool True

    it "returns an object that has no constructor property" $ do
      -- test case: S15.7.5_A1_T01
      jsEvalExpr "new Number().hasOwnProperty('constructor')"
        `shouldReturn` VBool False

    it "returns 0 for all whitespace" $
      jsEvalExpr "Number('\\u0009\\u000C\\u0020\\u00A0\\u000B\\u000A\\u000D\\u2028\\u2029\\u1680\\u180E\\u2000\\u2001\\u2002\\u2003\\u2004\\u2005\\u2006\\u2007\\u2008\\u2009\\u200A\\u202F\\u205F\\u3000')" `shouldReturn` VInt 0

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
