module Parse.Number.StringNumericLiteral where

import Text.Parsec hiding (many, optional, (<|>), crlf)
import Control.Applicative
import Data.Char
import qualified Data.Text as T
import Data.Text (Text)
import Data.Monoid
import Numeric (readHex, readOct)
import Parse.Types
import Parse.Lexical
import Data.Maybe
import Expr
import Parse.State
import Parse.Number.Common
import JSNum

import Debug.Trace


-- See note in Parse.Number concerning the two number
-- parsers Javascript provides.


-- ref 9.3.1
stringNumericLiteral :: JSParser (Either Integer Double)
stringNumericLiteral = whiteSpace >> strNumericLiteral

strNumericLiteral :: JSParser (Either Integer Double)
strNumericLiteral = try (string "0x" >> Left <$> hexIntegerLiteral)
                <|> strDecimalLiteral

strDecimalLiteral :: JSParser (Either Integer Double)
strDecimalLiteral = try (char '-' >> (negateEither <$> strUnsignedDecimalLiteral))
                <|> try ((char '+' >> strUnsignedDecimalLiteral))
                <|> strUnsignedDecimalLiteral
  where negateEither :: Either Integer Double -> Either Integer Double
        negateEither (Left i)  = Left (-i)
        negateEither (Right d) = Right (-d)

strUnsignedDecimalLiteral :: JSParser (Either Integer Double)
strUnsignedDecimalLiteral =
  (string "Infinity" >> return (Right jsInf))
  <|> (try $ do { intPart <- many1 decimalDigit
                ; char '.'
                ; fracPart <- many decimalDigit
                ; expPart <- optional exponentPart
                ; return (makeDecimal intPart fracPart expPart)
                })
  <|> (try $ do { char '.'
                ; fracPart <- many1 decimalDigit
                ; expPart <- optional exponentPart
                ; return (makeDecimal [] fracPart expPart)
                })
  <|> (try $ do { intPart <- many1 decimalDigit
                ; expPart <- optional exponentPart
                ; return (makeDecimal intPart [] expPart)
                })
