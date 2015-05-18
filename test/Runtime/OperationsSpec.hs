module Runtime.OperationsSpec where

import Test.Hspec

import Expr
import Eval
import Runtime.Types
import Runtime.Operations

spec :: Spec
spec = do
  describe "jsAdd" $ do
    it "adds two strings" $ do
      runtime (jsAdd (VStr "a") (VStr "b")) `shouldReturn` Right (VStr "ab")

    it "adds two numbers" $ do
      runtime (jsAdd (VNum 1) (VNum 3)) `shouldReturn` Right (VNum 4)

    it "adds a bool and a number" $ do
      runtime (jsAdd (VBool True) (VNum 1)) `shouldReturn` Right (VNum 2)


  describe "doubleEquals" $ do
    let fromRight (Right (VBool a)) = a
    let eq a b = runtime (doubleEquals id a b) >>= return . fromRight

    it "compares two numbers" $ do
      eq (VNum 1) (VNum 1) `shouldReturn` True
      eq (VNum 1) (VNum 2) `shouldReturn` False

    it "compares two strings" $ do
      eq (VStr "a") (VStr "a") `shouldReturn` True
      eq (VStr "a") (VStr "b") `shouldReturn` False

    it "compares undef and null" $ do
      eq VUndef VUndef `shouldReturn` True
      eq VUndef VNull  `shouldReturn` True
      eq VNull VUndef  `shouldReturn` True
      eq VNull VNull   `shouldReturn` True
