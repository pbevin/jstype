{-# LANGUAGE LambdaCase, OverloadedStrings #-}

module Runtime.ObjectSpec where

import Control.Monad.Except
import Test.Hspec
import Expectations
import Eval
import Runtime

valueToProp :: JSVal -> PropDesc JSVal
valueToProp a = dataPD a True True True

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

    it "refuses to overwrite a read-only non-configurable property" $ do
      result <- runtime' $ do
        obj <- newObject
        defineOwnProperty "a" (dataPD (VNum 1) False False False) True obj
        defineOwnProperty "a" (dataPD (VNum 2) True True True) True obj

      result `shouldBeError` TypeError

    it "will overwrite a read-only property if it is configurable" $ do
      result <- runtime' $ do
        obj <- newObject
        defineOwnProperty "a" (dataPD (VNum 1) False False True) True obj
        defineOwnProperty "a" (dataPD (VNum 2) True True True) True obj
        objGet "a" obj

      result `shouldBe` Right (VNum 2)

    it "refuses to add a new property to an object that isn't extensible" $ do
      result <- runtime' $ do
        obj <- newObject >>= objSetExtensible False
        defineOwnProperty "a" (valueToProp $ VNum 2) True obj

      result `shouldBeError` TypeError
