{-# LANGUAGE LambdaCase #-}

module Runtime.ObjectSpec where

import Control.Monad.Except
import Test.Hspec
import Expectations
import Eval
import Runtime

spec :: Spec
spec = do
  describe "objDefineOwnProperty" $ do
    it "refuses to overwrite a read-only property" $ do
      result <- runtime' $ do
        obj <- newObject;
        objDefineOwnProperty "a" (readOnlyProperty $ VNum 1) True obj
        objDefineOwnProperty "a" (valueToProp $ VNum 2) True obj

      result `shouldBeError` TypeError

      -- 1 `shouldBe` 2

      -- run (objDefineOwnProperty "a" (valueToProp $ VNum 2) True obj) `shouldError` "TypeError"
