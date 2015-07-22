{-# LANGUAGE OverloadedStrings #-}

module Builtins.ArraySpec where

import Test.Hspec
import Runtime
import Expectations
import Eval

spec :: Spec
spec = do
  describe "Constructor" $ do
    it "initializes an empty array" $ do
      jsEvalExpr "new Array().length" `shouldReturn` VInt 0

    it "initializes an array with several elements" $ do
      jsEvalExpr "new Array(1,2,3,4,5).length" `shouldReturn` VInt 5

    it "initializes an array of blank elements with a length" $ do
      jsEvalExpr "new Array(10).length" `shouldReturn` VInt 10
      



  describe "Array.prototype.reduce" $ do
    it "is a fold" $ do
      jsEvalExpr "['1','2','3','4','5'].reduce(function(a,b){return a+b})" `shouldReturn` VStr "12345"


    it "handles a negative length" $ do
      -- -4294967294 wraps round to 2. So in the first example, the function is called once,
      -- and in the second, it's called twice.
      jsEvalExpr "Array.prototype.reduce.call({ 1: 2, length: -4294967294 }, function() {return 5}, 99)"
        `shouldReturn` VInt 5
      jsEvalExpr "Array.prototype.reduce.call({ 2: 2, length: -4294967294 }, function() {return 5}, 99)"
        `shouldReturn` VInt 99


    it "throws a type error if there are no values and no default value" $ do
      evalE "Array.prototype.reduce.call([,,,,], function(){})" `shouldErrorOfType` TypeError
