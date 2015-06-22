module Parse ( parseJS
             , parseJS'
             , parseJS''
             , simpleParse
             , parseInFunction
             , parseExpr
             , parseNumber
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

parseNumber :: String -> JSNum
parseNumber str = case jsParse numberParser NotStrict False "" str of
  Right (Just num) -> num
  Right Nothing    -> 0
  Left err         -> jsNaN

jsParse :: JSParser a -> Strictness -> Bool -> SourceName -> String -> Either ParseError a
jsParse p strict inFunction name text = runP p (initialParseState strict inFunction) name text

numberParser :: JSParser (Maybe Double)
numberParser = do
  whiteSpace
  plusMinus <- optional (oneOf "+-")
  num <- optional number
  whiteSpace
  eof

  return $ case num of
    Nothing -> Nothing
    Just n -> Just $ if plusMinus == Just '-'
                     then negate n
                     else n
