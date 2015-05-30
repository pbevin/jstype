module Runtime.ConversionSpec where

import Test.QuickCheck
import Test.Hspec

import Runtime

spec :: Spec
spec = do
  describe "toBoolean" $ do
    it "is false for undef" $ toBoolean VUndef       `shouldBe` False
    it "is false for null"  $ toBoolean VNull        `shouldBe` False
    it "is false for 0"     $ toBoolean (VNum 0)     `shouldBe` False
    it "is false for -0"    $ toBoolean (VNum (-0))  `shouldBe` False
    it "is false for NaN"   $ toBoolean (VNum (0/0)) `shouldBe` False

  describe "strToNumber" $ do
    it "does basic parsing" $ do
      strToNumber "" `shouldBe` 0
      strToNumber " " `shouldBe` 0
      strToNumber " 12 " `shouldBe` 12
