module Parse.Number.Common where

import Text.Parsec hiding (many, optional, (<|>), crlf)
import Control.Applicative
import Data.Char
import qualified Data.Text as T
import Data.Text (Text)
import Data.Monoid
import Parse.Types
import Data.Maybe
import Expr
import Parse.State

import Debug.Trace

makeDecimal :: String -> String -> Maybe Integer -> Either Integer Double
makeDecimal intDigits [] Nothing = Left (read intDigits)
makeDecimal intDigits [] (Just e)
  | e < 0 = Right $ read (intDigits <> "e" <> show e)
  | otherwise = Left $ (readInt intDigits) * (10 ^ e)
makeDecimal intDigits fracDigits Nothing =
  makeDecimal intDigits fracDigits (Just 0)
makeDecimal [] fracDigits (Just exp) = Right $
  read ("0." <> fracDigits <> "e" <> show exp)
makeDecimal intDigits fracDigits (Just exp) = Right $
  read (intDigits <> "." <> fracDigits <> "e" <> show exp)

applyExponent :: Double -> Integer -> Double
applyExponent d e
  | e < 0     = d / 10 ^ (-e)
  | otherwise = d * 10 ^ e

decimalDigit :: JSParser Char
decimalDigit = satisfy isDigit

exponentPart :: JSParser Integer
exponentPart = try (oneOf "eE" >> signedInteger)

signedInteger :: JSParser Integer
signedInteger = try (char '-' >> negate <$> unsignedInteger)
            <|> try (char '+' >> unsignedInteger)
            <|> unsignedInteger

unsignedInteger :: JSParser Integer
unsignedInteger = readInt <$> many1 decimalDigit

readInt :: String -> Integer
readInt = readBase 10

hexIntegerLiteral :: JSParser Integer
hexIntegerLiteral = do
  digits <- many1 hexDigit
  return $ readBase 16 digits


octalIntegerLiteral :: JSParser Integer
octalIntegerLiteral = do
  digits <- many1 (satisfy isOctDigit)
  return $ readBase 8 digits

readBase :: Int -> String -> Integer
readBase b = foldl shift 0 . map readDigit
  where shift :: Integer -> Int -> Integer
        shift n d = (fromIntegral b) * n + (fromIntegral d)

readDigit :: Char -> Int
readDigit = go . toLower where
  go ch
    | '0' <= ch && ch <= '9' = ord ch - ord '0'
    | 'a' <= ch && ch <= 'z' = 10 + ord ch - ord 'a'
    | otherwise = error $ "Not a digit: " ++ [ch]
