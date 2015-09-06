{-# LANGUAGE OverloadedStrings #-}

module Parse.NumericLiteralSpec where

import Test.Hspec
import Test.QuickCheck
import Expectations

import Text.Parsec (eof)

import Data.Monoid
import qualified Data.Text as T
import Data.Text (Text)

import Decimal

import Expr

import Parse.Types
import Parse.State
import Parse.Number.NumericLiteral


parse :: JSParser a -> Text -> a
parse p input = case runp (p <* eof) input of
                      Left err -> error (show err)
                      Right val -> val

spec :: Spec
spec = do
  describe "numericLiteral" $ do
    describe "simple cases" $ do
      specify "x.y" $ parse numericLiteral "123.456" `shouldBe` Right 123.456
      specify "0e1" $ parse numericLiteral "0e1" `shouldBe` Left 0
      specify "0.e0" $ parse numericLiteral "0.e0" `shouldBe` Left 0
      specify "0E00" $ parse numericLiteral "0E00" `shouldBe` Left 0
      specify "1.1E-1" $ parse numericLiteral "1.1E-1" `shouldBe` Right 0.11
      specify "1.1" $ parse numericLiteral "1.1" `shouldBe` Right 1.1

    describe "limits" $ do
      specify "MIN_VALUE" $
        parse numericLiteral "5e-324" `shouldBe` Right 5e-324
      specify "MAX_VALUE" $
        parse numericLiteral "1.7976931348623157e+308" `shouldBe`
          Right 1.7976931348623157e+308
