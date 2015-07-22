module Parse ( parseJS
             , parseJS'
             , parseJS''
             , simpleParse
             , parseInFunction
             , parseExpr
             , parseNumber
             , parseNum
             , parseDecimal
             , ParseError) where

import Control.Applicative
import Text.Parsec hiding (optional)
import Data.Text (Text)
import Expr
import JSNum

import Parse.Types
import Parse.Lexical
import Parse.State
import Parse.Statements

import Debug.Trace

parseJS :: Text -> Either ParseError Program
parseJS str = parseJS' str ""

parseJS' :: Text -> String -> Either ParseError Program
parseJS' str filename = jsParse (whiteSpace >> prog <* eof) NotStrict False filename str

parseJS'' :: Text -> String -> Strictness -> Bool -> Either ParseError Program
parseJS'' str filename strict inFunction = jsParse (whiteSpace >> prog <* eof) strict inFunction filename str

parseInFunction :: Text -> Either ParseError Program
parseInFunction str = parseJS'' str "" NotStrict True

simpleParse :: Text -> Program
simpleParse str = case parseJS str of
  Right p  -> p
  Left err -> error (show err)

parseExpr :: Text -> Expr
parseExpr str = case jsParse (expr <* eof) NotStrict False "" str of
  Right e  -> e
  Left err -> error (show err)

jsParse :: JSParser a -> Strictness -> Bool -> SourceName -> Text -> Either ParseError a
jsParse p strict inFunction name text = runP p (initialParseState strict inFunction) name text

parseNumber :: Text -> JSNum
parseNumber str = case jsParse numberParser NotStrict False "" str of
  Right (Just num) -> num
  Right Nothing    -> 0
  Left err         -> jsNaN

parseNum :: Text -> Either Integer Double
parseNum str = case jsParse (numericParser num <* eof) NotStrict False "" str of
  Right (Just (Right dbl)) -> Right dbl
  Right (Just (Left int))  -> Left int
  Right Nothing            -> Left 0
  Left err                 -> Right jsNaN

parseDecimal :: Text -> Maybe JSNum
parseDecimal str = case jsParse decimalParser NotStrict False "" str of
  Left err -> Nothing
  Right v  -> v

numericParser :: Num a => JSParser a -> JSParser (Maybe a)
numericParser p = do
  whiteSpace
  plusMinus <- optional (oneOf "+-")
  num <- optional p
  whiteSpace

  return $ case num of
    Nothing -> Nothing
    Just n -> Just $ if plusMinus == Just '-'
                     then negate n
                     else n

instance (Num a, Num b) => Num (Either a b)
  where
    negate (Left a) = Left (negate a)
    negate (Right b) = Right (negate b)

numberParser, decimalParser :: JSParser (Maybe Double)
numberParser = numericParser number <* eof
decimalParser = numericParser decimal
