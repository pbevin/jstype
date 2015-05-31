module Runtime.OperationsSpec where

import Test.Hspec

import Data.Bits
import Expr
import Eval
import Runtime
import Expectations
import JSNum

fromRight :: Show a => Either a JSVal -> Bool
fromRight (Right (VBool b)) = b
fromRight v = error $ "Not a boolean: " ++ show v

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
    let eq a b = runtime (doubleEquals a b) >>= return . fromRight

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

    it "compares a number and a string" $ do
      eq (VNum 1) (VStr "1") `shouldReturn` True
      eq (VNum 1) (VStr "2") `shouldReturn` False
      eq (VStr "1") (VNum 1) `shouldReturn` True
      eq (VStr "2") (VNum 1) `shouldReturn` False

    it "compares a number and a bool" $ do
      eq (VNum 0) (VBool True) `shouldReturn` False
      eq (VNum 0) (VBool False) `shouldReturn` True
      eq (VNum 1) (VBool True) `shouldReturn` True
      eq (VNum 1) (VBool False) `shouldReturn` False
      eq (VNum 2) (VBool True) `shouldReturn` False
      eq (VNum 2) (VBool False) `shouldReturn` False
      eq (VBool True) (VNum 0) `shouldReturn` False
      eq (VBool False) (VNum 0) `shouldReturn` True
      eq (VBool True) (VNum 1) `shouldReturn` True
      eq (VBool False) (VNum 1) `shouldReturn` False
      eq (VBool True) (VNum 2) `shouldReturn` False
      eq (VBool False) (VNum 2) `shouldReturn` False

  describe "tripleEquals" $ do
    let eqq a b = runtime (doubleEquals a b) >>= return . fromRight

    it "compares two numbers" $ do
      eqq (VNum 1) (VNum 1) `shouldReturn` True
      eqq (VNum 1) (VNum 2) `shouldReturn` False
      eqq (VNum $ 1/0) (VNum $ 1/0) `shouldReturn` True

  describe "fmod" $ do
    it "is handles + and - in both positions" $ do
      JSNum 101 `fmod` JSNum 51 `shouldBe` JSNum 50
      JSNum 101 `fmod` JSNum (-51) `shouldBe` JSNum 50
      JSNum (-101) `fmod` JSNum 51 `shouldBe` JSNum (-50)
      JSNum (-101) `fmod` JSNum (-51) `shouldBe` JSNum (-50)

    it "is valid for edge cases" $ do
      let jsInfinity = JSNum (1/0)
          jsNaN = JSNum (0/0)
          isJsNaN (JSNum a) = isNaN a
      jsNaN `fmod` jsInfinity `shouldSatisfy` isJsNaN
      1 `fmod` (-jsInfinity) `shouldBe` 1

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
      let f a b = a + b
      runtime (mathFunc2 f VUndef [VNum 3.14, VNum 2.72]) `shouldReturn` Right (VNum 5.86)

    describe "Math.pow" $ do
      let inf = 1/0
      let nan = 0/0

      it "raises one number to the power of another" $ do
        pow 3 2 `shouldBe` 9

      it "is NaN for +/-1 and +/-Infinity" $ do
        pow 1 (-inf) `shouldSatisfy` isNaN
        pow (-1) (-inf) `shouldSatisfy` isNaN
        pow 1 inf `shouldSatisfy` isNaN
        pow (-1) inf `shouldSatisfy` isNaN

      it "is +Infinity when x is +0 and y < 0" $ do
        pow 0 (-1) `shouldBe` inf
        pow 0 (-2) `shouldBe` inf

      it "is -Infinity when x is -0, y < 0, and y is an odd integer" $ do
        -- yes indeed
        pow (-0) (-11111) `shouldBe` -inf

      it "is +Infinity when x is -0, y < 0, and y not an odd integer" $ do
        pow (-0) (-2) `shouldBe` inf
        pow (-0) (-inf) `shouldBe` inf
        pow (-0) (-1.79e308) `shouldBe` inf

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
