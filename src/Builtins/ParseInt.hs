module Builtins.ParseInt (parseInt) where

import Control.Applicative
import Data.Char (toLower)
import Data.List (elemIndex)
import Data.Maybe
import Unicode
import JSNum

-- ref 15.1.2.2
parseInt :: String -> Maybe Int -> Maybe Double
parseInt str radix = do
  radix'    <- fixRadix (fromMaybe 0 radix)
  (str', r) <- checkBase16 str radix'
  (sign, s) <- fixStr str' r

  return $ sign * numberFromDigits 0 (fromIntegral r) (findDigit r) s


fixRadix :: Int -> Maybe Int
fixRadix radix
  | r == 0    = Just 10
  | r < 2     = Nothing
  | r > 36    = Nothing
  | otherwise = Just r
  where r = radix `mod` (2^32)

checkBase16 :: String -> Int -> Maybe (String, Int)
checkBase16 str radix = case str of
  '0':'x':hex -> Just (hex, 16)
  '0':'X':hex -> Just (hex, 16)
  other       -> Just (other, radix)

fixStr :: String -> Int -> Maybe (Double, String)
fixStr str radix =
  let fixup (sign, s) = case takeWhile (isDigit radix) s of
        "" -> Nothing
        s' -> Just (sign, s')
  in fixup $ case dropWhile isJsSpace str of
    '-':num -> (-1, num)
    '+':num -> ( 1, num)
    num     -> ( 1, num)


numberFromDigits :: Double -> Double -> (Char -> Maybe Double) -> String -> Double
numberFromDigits acc radix digitToInt str = case str of
  []      -> acc
  ch:rest -> case digitToInt ch of
    Nothing -> acc
    Just d  -> numberFromDigits (acc*radix+d) radix digitToInt rest



isDigit :: Int -> Char -> Bool
isDigit r = isJust . findDigit r

findDigit :: Int -> Char -> Maybe Double
findDigit r ch = fromIntegral <$> (lower <|> upper)
  where lower = find allDigitsLC
        upper = find allDigitsUC
        find = elemIndex ch . take r

allDigitsLC :: String
allDigitsLC = map toLower allDigitsUC

allDigitsUC :: String
allDigitsUC = ['0'..'9'] ++ ['A'..'Z']
