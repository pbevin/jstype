module RuntimeSpec where

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
