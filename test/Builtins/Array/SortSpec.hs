{-# LANGUAGE OverloadedStrings #-}

module Builtins.Array.SortSpec where

import Test.Hspec
import Expectations

spec :: Spec
spec = do
  describe "Array.prototype.sort" $ do
    it "sorts a simple array" $ do
      runJStr "console.log([2,1,4,3].sort().toString())" `shouldReturn`
        Right "1,2,3,4\n"

    it "sorts a simple array with a compare function" $ do
      runJStr "console.log([2,1,4,3].sort(function(x, y) { return y-x }).toString())" `shouldReturn`
        Right "4,3,2,1\n"
