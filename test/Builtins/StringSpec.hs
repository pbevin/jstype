{-# LANGUAGE OverloadedStrings #-}

module Builtins.StringSpec where

import Test.Hspec
import Builtins.String
import Eval
import Runtime


spec :: Spec
spec = do
  describe "replace" $ do
    it "replaces based on a function" $ do
      jsEvalExpr "'test string'.replace('string', function(){return 'passed'})"
        `shouldReturn` VStr "test passed"

    it "does not molest the string if the seach text is not found" $ do
      jsEvalExpr "'test string'.replace('failed', function(){return 'passed'})"
        `shouldReturn` VStr "test string"


  describe "search" $ do
    it "finds the match index of a regex" $ do
      jsEvalExpr "'test string'.search(/r/)" `shouldReturn` VNum 7

    it "converts a string to regexp" $ do
      jsEvalExpr "'test string'.search('s.*s')" `shouldReturn` VNum 2
