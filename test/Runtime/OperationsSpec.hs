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
