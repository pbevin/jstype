module Builtins.ParseIntSpec where

import Test.Hspec
import Builtins.ParseInt

spec :: Spec
spec = do
  describe "parseInt" $ do
    it "parses simple numbers in base 10" $ do
      parseInt "0"    Nothing `shouldBe` Just 0
      parseInt "1"    Nothing `shouldBe` Just 1
      parseInt "4"    Nothing `shouldBe` Just 4
      parseInt "10"   Nothing `shouldBe` Just 10
      parseInt "1234" Nothing `shouldBe` Just 1234

    it "parses + and - numbers in base 10" $ do
      parseInt "+1"   Nothing `shouldBe` Just 1
      parseInt "-1"   Nothing `shouldBe` Just (-1)
      parseInt "-190" Nothing `shouldBe` Just (-190)
      parseInt "+0"   Nothing `shouldBe` Just 0
      parseInt "-0"   Nothing `shouldBe` Just 0

    it "ignores leading and trailing white space" $ do
      parseInt " 1 "  Nothing `shouldBe` Just 1

    it "returns NaN when there is no number" $ do
      parseInt ""     Nothing `shouldBe` Nothing

    it "wraps the radix at 32 bits" $
      parseInt "11" (Just 2) `shouldBe` Just 3

    it "wraps the radix at 32 bits" $
      parseInt "11" (Just $ 2^32+2) `shouldBe` Just 3

    it "detects a hex number" $ do
      parseInt "0xff" Nothing   `shouldBe` Just 255
      parseInt "0x2A" (Just 16) `shouldBe` Just 42
      parseInt "0xc0" (Just 10) `shouldBe` Just 192
      parseInt "0X1"  Nothing   `shouldBe` Just 1

