module Decimal where

import Test.QuickCheck
import Data.Char (digitToInt)
import Data.Text (Text)
import qualified Data.Text as T


newtype DecimalDigit = DecimalDigit { fromDigit :: Char } deriving (Show, Eq)
instance Arbitrary DecimalDigit where
  arbitrary = DecimalDigit <$> choose ('0', '9')

newtype DecimalNumber = DecimalNumber { fromDecimal :: Text } deriving (Show, Eq)
instance Arbitrary DecimalNumber where
  arbitrary = DecimalNumber . T.pack . map fromDigit <$> listOf1 arbitrary

simpleParseDecimal :: Text -> Integer
simpleParseDecimal = foldl shift 0 . map (fromIntegral . digitToInt) . T.unpack
  where shift n d = n*10+d

withDecimal :: (Text -> Bool) -> (DecimalNumber -> Bool)
withDecimal p d = p (fromDecimal d)

decimalProperty = property . withDecimal
