module Parse.Number.NumericLiteral where

import Text.Parsec hiding (many, optional, (<|>), crlf)
import Control.Applicative
import Data.Char
import qualified Data.Text as T
import Data.Text (Text)
import Data.Monoid
import Numeric (readHex, readOct)
import Parse.Types
import Data.Maybe
import Expr
import Unicode
import Parse.State
import Parse.Lexical
import Parse.Number.Common

import Debug.Trace


-- See note in Parse.Number concerning the two number
-- parsers Javascript provides.


-- ref 7.8.3
numericLiteral :: JSParser (Either Integer Double)
numericLiteral = lexeme (hexLiteral <|> octalIfNotStrictMode <|> decimalLiteral)

hexLiteral = try (char '0' >> oneOf "xX" >> Left <$> hexIntegerLiteral)
  
decimalLiteral :: JSParser (Either Integer Double)
decimalLiteral = (try $ do { intPart <- decimalIntegerLiteral
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
             <|> (try $ do { intPart <- decimalIntegerLiteral
                           ; expPart <- optional exponentPart
                           ; return (makeDecimal intPart [] expPart)
                           })

decimalIntegerLiteral :: JSParser String
decimalIntegerLiteral = string "0"
                    <|> do { first <- oneOf "123456789" 
                           ; rest <- many decimalDigit
                           ; return (first : rest)
                           }

decimalFraction :: JSParser (Either Integer Double)
decimalFraction = do
  fracPart <- frac <$> many1 decimalDigit
  expPart <- optional exponentPart
  return $ Right (applyExponent fracPart $ fromMaybe 0 expPart)

decimalLiteral2 :: JSParser (Either Integer Double)
decimalLiteral2 = undefined

frac :: String -> Double
frac = go 0.1 0
  where go _ acc [] = acc
        go mult acc (x:xs) = go (mult / 10) (acc + mult * d) xs
          where d = fromIntegral (digitToInt x)

octalIfNotStrictMode :: JSParser (Either Integer Double)
octalIfNotStrictMode = ifNotStrict $ try $
  lookAhead (char '0' >> oneOf "1234567") >> Left <$> octalIntegerLiteral
