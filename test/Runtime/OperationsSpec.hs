module Runtime.OperationsSpec where

import Test.Hspec

import Data.Bits
import Expr
import Eval
import Runtime.Types
import Runtime.Operations

spec :: Spec
spec = do
  describe "jsAdd" $ do
    let testAdd a b = runtime $ jsAdd a b
    it "adds two strings" $ do
      testAdd (VStr "a") (VStr "b") `shouldReturn` Right (VStr "ab")

    it "adds two numbers" $ do
      testAdd (VNum 1) (VNum 3) `shouldReturn` Right (VNum 4)

    it "adds a bool and a number" $ do
      testAdd (VBool True) (VNum 1) `shouldReturn` Right (VNum 2)

    it "adds a number and a string" $ do
      testAdd (VNum 1) (VStr "3") `shouldReturn` Right (VStr "13")


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

  describe "bitwise" $ do
    it "can logical-and two numbers" $ do
      runtime (bitwise (.&.) (VNum 3) (VNum 26)) `shouldReturn` Right (VNum 2)


  describe "Math" $ do
    it "can get the maximum" $ do
      runtime (mathMaxFunc maximum VUndef [VNum 1, VNum 3, VNum 2]) `shouldReturn` Right (VNum 3)

    it "can run an arbitrary floating point function" $ do
      let f a = a * 2.5
      runtime (mathFunc f VUndef [VNum 3]) `shouldReturn` Right (VNum 7.5)

    it "can run a 2-arg FP function" $ do
      let pow a b = a ** b
      runtime (mathFunc2 pow VUndef [VNum 3, VNum 2]) `shouldReturn` Right (VNum 9)
