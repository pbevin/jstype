{-# LANGUAGE OverloadedStrings #-}

module Builtins.StringSpec where

import Test.Hspec
import Expectations
import Builtins.String
import Eval
import Runtime


spec :: Spec
spec = do
  describe "The String constructor" $ do
    it "sets the primitive value to a string" $ do
      jsEvalExpr "typeof (new String(false).toString())" `shouldReturn` VStr "string"

  describe "The String class" $ do
    it "has prototype Function.prototype" $ do
      jsEvalExpr "Function.prototype.isPrototypeOf(String)" `shouldReturn` VBool True

  describe ".length" $ do
    it "is not writable" $ do
      runJStr "var a = new String('abc'); a.length = 9; console.log(a.length)" `shouldReturn` Right "3\n"

    it "is not writable via with()" $ do
      runJStr "var a = new String('abc'); with(a) { length = 9 }; console.log(a.length)" `shouldReturn` Right "3\n"

  describe "String.prototype.replace" $ do
    it "replaces based on a function" $ do
      jsEvalExpr "'test string'.replace('string', function(){return 'passed'})"
        `shouldReturn` VStr "test passed"

    it "does not molest the string if the seach text is not found" $ do
      jsEvalExpr "'test string'.replace('failed', function(){return 'passed'})"
        `shouldReturn` VStr "test string"

  describe "String.prototype.search" $ do
    it "finds the match index of a regex" $ do
      jsEvalExpr "'test string'.search(/r/)" `shouldReturn` VNum 7

    it "converts a string to regexp" $ do
      jsEvalExpr "'test string'.search('s.*s')" `shouldReturn` VNum 2

  describe "String.prototype.charAt" $ do
    it "returns a single character" $ do
      jsEvalExpr "'abc'.charAt(2)" `shouldReturn` VStr "c"

    it "returns the empty string when out of range" $ do
      jsEvalExpr "'abc'.charAt(3)" `shouldReturn` VStr ""
      jsEvalExpr "'abc'.charAt(-1)" `shouldReturn` VStr ""


  describe "String.prototype.lastIndexOf" $ do
    it "finds the rightmost match if one exists" $ do
      jsEvalExpr "'abcabc'.lastIndexOf('b')" `shouldReturn` VNum 4
      jsEvalExpr "'abcabc'.lastIndexOf('a')" `shouldReturn` VNum 3
      jsEvalExpr "'abcabc'.lastIndexOf('cab')" `shouldReturn` VNum 2

    it "returns -1 if it does not find a match" $ do
      jsEvalExpr "'abcabc'.lastIndexOf('x')" `shouldReturn` VNum (-1)
