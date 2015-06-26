module RuntimeSpec where

import Control.Monad.Trans
import Test.Hspec
import Runtime
import Eval
import Expectations

spec :: Spec
spec = do
  describe "createArray" $ do
    let a = VStr "a"

    it "has the right length" $ do
      arrayLength [] `shouldReturnResult` VNum 0
    it "has the right length" $ do
      arrayLength [Just (VStr "a")] `shouldReturnResult` VNum 1
    it "has the right length" $ do
      arrayLength [Nothing] `shouldReturnResult` VNum 1
    it "has the right length" $ do
      arrayLength [Nothing, Nothing] `shouldReturnResult` VNum 2
    it "has the right length" $ do
      arrayLength [Just a, Nothing, Nothing] `shouldReturnResult` VNum 3

  describe "Object.prototype" $ do
    it "is the global object prototype" $ runjs $ do
      globalObject <- getGlobalObject
      prototype <- getGlobalObjectPrototype
      actual <- objGet "prototype" globalObject
      liftIO $ do
        actual `shouldBe` (VObj prototype)

    it "has a constructor property" $ runjs $ do
      object <- objGet "Object" =<< getGlobalObject
      prototype <- getGlobalObjectPrototype
      actual <- objGet "constructor" prototype
      liftIO $ do
        actual `shouldBe` object


  describe "A new object" $ do
    it "has a constructor property" $ runjs $ do
      object <- objGet "Object" =<< getGlobalObject
      obj <- newObject
      actual <- objGet "constructor" obj
      liftIO $ do
        actual `shouldBe` object
