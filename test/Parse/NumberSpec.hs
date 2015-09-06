{-# LANGUAGE OverloadedStrings #-}

module Parse.NumberSpec where

import Test.Hspec
import Expectations
import Test.QuickCheck

import Data.Monoid
import Text.Parsec
import qualified Data.Text as T
import Data.Text (Text)

import Decimal

import Expr

import Parse.Number

spec :: Spec
spec = do

  describe "parseStrNum" $ do
    it "parses empty string as 0" $ do
      parseStrNum "" `shouldBe` Left 0

    it "parses white space as 0" $ do
      parseStrNum "  " `shouldBe` Left 0

    describe "ignores white space" $ do
      specify "around" $ parseStrNum " 1 " `shouldBe` Left 1
      specify "after" $ parseStrNum "2 " `shouldBe` Left 2
      specify "before" $ parseStrNum " 3" `shouldBe` Left 3
      specify "without" $ parseStrNum "4" `shouldBe` Left 4

    it "parses a + prefix" $ do
      parseStrNum "+123" `shouldBe` Left 123

    it "parses a - prefix" $ do
      parseStrNum "-123" `shouldBe` Left (-123)

    it "parses a string of decimal digits" $ do
      parseStrNum "12345678901234567890" `shouldBe` Left 12345678901234567890

    it "parses a number plus positive exponent as an int" $ do
      parseStrNum "123e3" `shouldBe` Left 123000

    it "parses a number plus negative exponent as a double" $ do
      parseStrNum "123e-3" `shouldBe` Right 0.123

    it "parses numbers to 1000" $ do
      let nums = [0..1000]
      map (parseStrNum . T.pack . show) nums `shouldBe` map Left nums

    it "parses a decimal string" $ decimalProperty $ \n ->
      parseStrNum n == Left (simpleParseDecimal n)

    it "parses a decimal string followed by ." $ decimalProperty $ \n ->
      parseStrNum n == parseStrNum (T.snoc n '.')

    it "parses exponents E and e the same way" $ decimalProperty $ \n ->
      parseStrNum (n <> "e-5") == parseStrNum (n <> "E-5")

    it "treats e1 as multiplication by 10" $ decimalProperty $ \n ->
       let r = fromIntegral . simpleParseDecimal $ n
        in parseStrNum (n <> "e1") == Left (10 * r)
