{-# LANGUAGE LambdaCase #-}

module Runtime.FunctionSpec where

import Test.Hspec

import Runtime
import Eval
import Expectations

hasInstance :: Shared JSObj -> JSVal -> Runtime Bool
hasInstance a b = do
  hasInstanceMethod <$> deref a >>= \case
    Just method -> method a b
    Nothing -> error $ "No hasInstance method on " ++ show a


spec :: Spec
spec = do
  describe "hasInstance" $ do
    it "is true for Array and an array object" $ do
      result <- runtime' $ do
        arrayType <- getGlobalObject >>= objGet "Array"
        array <- createArray [Just (VStr "a")]
        let VObj cls = arrayType
        cls `hasInstance` array

      result `shouldBeResult` True

    it "is true for TypeError and a type error" $ do
      result <- runtime' $ do
        errType <- getGlobalObject >>= objGet "TypeError"
        err <- createError TypeError (VStr "bad type")
        let VObj cls = errType
        cls `hasInstance` err

      result `shouldBeResult` True
