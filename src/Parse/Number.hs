{-# LANGUAGE OverloadedStrings #-}

module Parse.Number (parseStrNum, parseDecimal) where

import Text.Parsec hiding (optional)
import Data.Text (Text)
import qualified Data.Text as T
import Parse.Types
import Parse.State
import Expr
import JSNum
import Unicode

import Parse.Number.NumericLiteral
import Parse.Number.StringNumericLiteral
import Parse.Lexical
import Parse.State


-- import Parse.Number.StringNumericLiteral

-- Javascript has not one, but two number parsers: NumericLiteral and
-- StringNumericLiteral.
--
-- NumericLiteral, defined in 7.8.3, is used for parsing program text. It accepts
-- decimal, hex, and octal.
--
-- StringNumericLiteral, defined in 9.8.1, is used for converting JS strings to
-- numbers at runtime, via the internal ToNumber primitive. It only accepts
-- decimal and hex numbers.
--
-- Section 9.8.1 includes a comparison of the two parsers:
--   - A StringNumericLiteral may be preceded and/or
--     followed by white space and/or line terminators.
--
--   - A StringNumericLiteral that is decimal may have
--     any number of leading 0 digits.
--
--   - A StringNumericLiteral that is decimal may be
--     preceded by + or - to indicate its sign.
--
--   - A StringNumericLiteral that is empty or contains
--     only white space is converted to +0.

runParse :: JSParser a -> Text -> Maybe a
runParse p input = case runp p input of
                     Left _err -> Nothing
                     Right val -> Just val

parseStrNum :: Text -> Either Integer Double
parseStrNum = ifAllSpace (Left 0) parseStrNum'
  where
    parseStrNum' input = case runParse (stringNumericLiteral <* whiteSpace <* eof) input of
                           Nothing  -> Right jsNaN
                           Just val -> val

-- for parseFloat()
-- ref 15.1.2.3
parseDecimal :: Text -> Double
parseDecimal = ifAllSpace jsNaN parseDecimal'
  where
    parseDecimal' input = case runParse strDecimalLiteral input of
                             Nothing          -> jsNaN
                             Just (Left int)  -> fromIntegral int
                             Just (Right dbl) -> dbl


ifAllSpace :: a -> (Text -> a) -> (Text -> a)
ifAllSpace a f t = let t' = T.dropWhile isJsSpace t
                    in if t' == ""
                       then a
                       else f t'
