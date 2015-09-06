{-# LANGUAGE OverloadedStrings #-}

module Builtins.ParseFloatSpec where

import Test.QuickCheck
import Test.Hspec
import Builtins.ParseFloat
import Decimal
import JSNum
import qualified Data.Text as T

spec :: Spec
spec = do
  describe "parseFloat" $ do
    it "parses simple numbers in base 10" $ do
      parseFloat "0"    `shouldBe` 0
      parseFloat "1"    `shouldBe` 1
      parseFloat "4"    `shouldBe` 4
      parseFloat "10"   `shouldBe` 10
      parseFloat "1234" `shouldBe` 1234

    it "ignores leading whitespace" $ do
      parseFloat "  1.23" `shouldBe` 1.23

    it "does not interpret hexadecimal literals" $ do
      parseFloat "0x23" `shouldBe` 0

    it "allows a leading zero" $ property $ withDecimal $ \n ->
      parseFloat (T.cons '0' n) == parseFloat n

    it "allows several leading zero" $ property $ withDecimal $ \n ->
      parseFloat (T.append "00000" n) == parseFloat n

    it "parses + and - numbers in base 10" $ do
      parseFloat "+1"   `shouldBe` 1
      parseFloat "-1"   `shouldBe` (-1)
      parseFloat "-190" `shouldBe` (-190)
      parseFloat "+0"   `shouldBe` 0
      parseFloat "-0"   `shouldBe` 0

    it "ignores leading and trailing white space" $ do
      parseFloat " 1 "  `shouldBe` 1

    it "returns Nothing when there is no number" $ do
      parseFloat ""     `shouldSatisfy` isNaN
      parseFloat "+"    `shouldSatisfy` isNaN
      parseFloat "-"    `shouldSatisfy` isNaN

    it "does not detect hex numbers" $ do
      parseFloat "0x1"  `shouldBe` 0

    it "parses the longest prefix" $ do
      parseFloat "0x"   `shouldBe` 0
      parseFloat "1ex"  `shouldBe` 1
      parseFloat "Infinity1" `shouldBe` jsInf
      parseFloat "01.string" `shouldBe` 1
