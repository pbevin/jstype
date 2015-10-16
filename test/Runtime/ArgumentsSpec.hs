{-# LANGUAGE OverloadedStrings #-}

module Runtime.ArgumentsSpec where

import Control.Monad.Trans
import Test.Hspec
import Runtime
import Eval
import Expectations

spec :: Spec
spec = do
  describe "An arguments object" $ do
    it "has a constructor property" $ runjs $ do
      -- language/arguments-object/S10.6_A2
      args <- jsEval "(function() { return arguments.constructor.prototype; })()"
      objectPrototype <- getGlobalObjectPrototype

      liftIO $ do
        args `shouldBe` (VObj objectPrototype)

    it "reads values from the function environment" $ do
      jsEvalExpr "(function(a) {        return arguments[0] })(3)" `shouldReturn` VInt 3
    it "reads updated value when variable is updated" $ do
      jsEvalExpr "(function(a) { a = 5; return arguments[0] })(4)" `shouldReturn` VInt 5

    it "writes values to the environment" $ do
      jsEvalExpr "(function(a) { arguments[0] = 4; return a; })(3)" `shouldReturn` VInt 4

    it "does not update the values in strict mode" $ do
      jsEvalExpr "(function(a) { 'use strict'; arguments[0] = 4; return a; })(3)" `shouldReturn` VInt 3

    it "always returns the original values in strict mode" $ do
      jsEvalExpr "(function(a) { 'use strict';        return arguments[0] })(7)" `shouldReturn` VInt 7
      jsEvalExpr "(function(a) { 'use strict'; a = 9; return arguments[0] })(8)" `shouldReturn` VInt 8

    it "includes arguments passed, even if not declared" $ do
      jsEvalExpr("(function () { return arguments[0] })(78)") `shouldReturn` VInt 78

    it "has a length property" $ do
      jsEvalExpr "(function() { return arguments.length; })(1,2,3)" `shouldReturn` VInt 3
