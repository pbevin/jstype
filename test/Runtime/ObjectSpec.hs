{-# LANGUAGE LambdaCase #-}

module Runtime.ObjectSpec where

import Control.Monad.Except
import Test.Hspec
import Expectations
import Eval
import Runtime

spec :: Spec
spec = do
  describe "defineOwnProperty" $ do
    it "sets a property on an object" $ do
      result <- runtime' $ do
        obj <- newObject
        defineOwnProperty "a" (valueToProp $ VNum 42) True obj
        objGet "a" obj

      result `shouldBe` Right (VNum 42)

    it "overwrites an existing property" $ do
      result <- runtime' $ do
        obj <- newObject
        defineOwnProperty "a" (valueToProp $ VNum 42) True obj
        defineOwnProperty "a" (valueToProp $ VNum 54) True obj
        objGet "a" obj

      result `shouldBe` Right (VNum 54)

    it "refuses to overwrite a read-only property" $ do
      result <- runtime' $ do
        obj <- newObject
        defineOwnProperty "a" (readOnlyProperty $ VNum 1) True obj
        defineOwnProperty "a" (valueToProp $ VNum 2) True obj

      result `shouldBeError` TypeError

    it "refuses to add a new property to an object that isn't extensible" $ do
      result <- runtime' $ do
        obj <- newObject >>= objSetExtensible False
        defineOwnProperty "a" (valueToProp $ VNum 2) True obj

      result `shouldBeError` TypeError
