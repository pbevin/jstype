module Builtins.ParseFloatSpec where

import Test.Hspec
import Builtins.ParseFloat
import JSNum

spec :: Spec
spec = do
  describe "parseFloat" $ do
    it "parses simple numbers in base 10" $ do
      parseFloat "0"    `shouldBe` Just 0
      parseFloat "1"    `shouldBe` Just 1
      parseFloat "4"    `shouldBe` Just 4
      parseFloat "10"   `shouldBe` Just 10
      parseFloat "1234" `shouldBe` Just 1234

    it "parses + and - numbers in base 10" $ do
      parseFloat "+1"   `shouldBe` Just 1
      parseFloat "-1"   `shouldBe` Just (-1)
      parseFloat "-190" `shouldBe` Just (-190)
      parseFloat "+0"   `shouldBe` Just 0
      parseFloat "-0"   `shouldBe` Just 0

    it "ignores leading and trailing white space" $ do
      parseFloat " 1 "  `shouldBe` Just 1

    it "returns Nothing when there is no number" $ do
      parseFloat ""     `shouldBe` Nothing
      parseFloat "+"    `shouldBe` Nothing
      parseFloat "-"    `shouldBe` Nothing

    it "does not detect hex numbers" $ do
      parseFloat "0x1"  `shouldBe` Just 0

    it "parses the longest prefix" $ do
      parseFloat "0x"   `shouldBe` Just 0
      parseFloat "1ex"  `shouldBe` Just 1
      parseFloat "Infinity1" `shouldBe` Just jsInf
      parseFloat "01.string" `shouldBe` Just 1
