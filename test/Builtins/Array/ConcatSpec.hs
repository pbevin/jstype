{-# LANGUAGE OverloadedStrings #-}

module Builtins.Array.ConcatSpec where

import Test.Hspec
import Runtime
import Expectations
import Eval

spec :: Spec
spec = do
  describe "Array.prototype.concat" $ do
    it "adds one array to another" $ do
      jsEvalExpr "[1,2,3].concat([4,5,6]).toString()"
        `shouldReturn` VStr "1,2,3,4,5,6"

    specify "with explicit array creation" $ do
      runJStr "a = new Array(); a[0] = 2; b = new Array(); b[0] = 3; console.log(a.concat(b).toString());" `shouldReturn` Right "2,3\n"
