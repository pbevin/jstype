module Parse.NumberCommonSpec where

import Test.Hspec

import Parse.Number.Common

main = hspec spec

spec :: Spec
spec = do
  describe "makeDecimal" $ do
    describe "without an exponent" $ do
      specify "0" $ makeDecimal "0" "" Nothing `shouldBe` Left 0
      specify "123" $ makeDecimal "123" "" Nothing `shouldBe` Left 123
      specify "0.123" $ makeDecimal "" "123" Nothing `shouldBe` Right 0.123
      specify "1.1" $ makeDecimal "1" "1" Nothing `shouldBe` Right 1.1
      specify "0.1" $ makeDecimal "0" "1" Nothing `shouldBe` Right 0.1
      specify ".1" $ makeDecimal "" "1" Nothing `shouldBe` Right 0.1
