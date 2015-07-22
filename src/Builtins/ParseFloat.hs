module Builtins.ParseFloat (parseFloat) where

import Control.Applicative
import Data.Char (toLower)
import Data.List (elemIndex)
import qualified Data.Text as T
import Data.Maybe
import Unicode
import JSNum
import Runtime

-- ref 15.1.2.3
parseFloat :: String -> Maybe Double
parseFloat str =
  case dropWhile isJsSpace str of
  "" -> Nothing
  "Infinity" -> Just jsInf
  '+':rest   -> parseFloat rest
  '-':rest   -> negate <$> parseFloat rest
  _          -> parseDecimal (T.pack str)
