module RuntimeSpec where

import Control.Monad.Trans
import Test.Hspec
import Runtime
import Eval
import Expectations

unObj :: JSVal -> Shared JSObj
unObj (VObj obj) = obj

arrayLength :: [Maybe JSVal] -> IO (Either JSError JSVal)
arrayLength xs = runtime' $ do
  arr <- createArray xs
  objGet "length" (unObj arr)

runjs :: Runtime a -> IO ()
runjs a = runtime a >> return ()

jsEval :: String -> Runtime JSVal
jsEval = liftIO . jsEvalExpr

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
      object <- getGlobalObject
      prototype <- getGlobalObjectPrototype
      actual <- objGet "prototype" object
      liftIO $ do
        actual `shouldBe` (VObj prototype)

    it "has a constructor property" $ runjs $ do
      object <- getGlobalObject
      prototype <- getGlobalObjectPrototype
      actual <- objGet "constructor" prototype
      liftIO $ do
        actual `shouldBe` (VObj object)


  describe "A new object" $ do
    it "has a constructor property" $ runjs $ do
      object <- getGlobalObject
      obj <- newObject
      actual <- objGet "constructor" obj
      liftIO $ do
        actual `shouldBe` (VObj object)

  describe "An arguments object" $ do
    it "has a constructor property" $ runjs $ do
      -- language/arguments-object/S10.6_A2
      args <- jsEval "(function() { return arguments.constructor.prototype; })()"
      objectPrototype <- getGlobalObjectPrototype

      liftIO $ do
        args `shouldBe` (VObj objectPrototype)
