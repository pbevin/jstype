module Parse ( parseJS
             , parseJS'
             , parseJS''
             , simpleParse
             , parseInFunction
             , parseExpr
             , parseNumber
             , parseDecimal
             , ParseError) where

import Control.Applicative
import Text.Parsec hiding (optional)
import Expr
import JSNum

import Parse.Types
import Parse.Lexical
import Parse.State
import Parse.Statements

import Debug.Trace

parseJS :: String -> Either ParseError Program
parseJS str = parseJS' str ""

parseJS' :: String -> String -> Either ParseError Program
parseJS' str filename = jsParse (whiteSpace >> prog <* eof) NotStrict False filename str

parseJS'' :: String -> String -> Strictness -> Bool -> Either ParseError Program
parseJS'' str filename strict inFunction = jsParse (whiteSpace >> prog <* eof) strict inFunction filename str

parseInFunction ::String -> Either ParseError Program
parseInFunction str = parseJS'' str "" NotStrict True

simpleParse :: String -> Program
simpleParse str = case parseJS str of
  Right p  -> p
  Left err -> error (show err)

parseExpr :: String -> Expr
parseExpr str = case jsParse (expr <* eof) NotStrict False "" str of
  Right e  -> e
  Left err -> error (show err)

jsParse :: JSParser a -> Strictness -> Bool -> SourceName -> String -> Either ParseError a
jsParse p strict inFunction name text = runP p (initialParseState strict inFunction) name text

parseNumber :: String -> JSNum
parseNumber str = case jsParse numberParser NotStrict False "" str of
  Right (Just num) -> num
  Right Nothing    -> 0
  Left err         -> jsNaN

parseDecimal :: String -> Maybe JSNum
parseDecimal str = case jsParse decimalParser NotStrict False "" str of
  Left err -> Nothing
  Right v  -> v

numericParser :: JSParser Double -> JSParser (Maybe Double)
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


numberParser, decimalParser :: JSParser (Maybe Double)
numberParser = numericParser number <* eof
decimalParser = numericParser decimal
