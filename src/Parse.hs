module Parse ( parseJS
             , parseJS'
             , parseJS''
             , simpleParse
             , parseExpr
             , ParseError) where

import Control.Applicative
import Text.Parsec
import Expr

import Parse.Types
import Parse.Lexical
import Parse.State
import Parse.Statements

import Debug.Trace

parseJS :: String -> Either ParseError Program
parseJS str = parseJS' str ""

parseJS' :: String -> String -> Either ParseError Program
parseJS' str filename = jsParse (whiteSpace >> prog <* eof) NotStrict filename str

parseJS'' :: String -> String -> Strictness -> Either ParseError Program
parseJS'' str filename strict = jsParse (whiteSpace >> prog <* eof) strict filename str

simpleParse :: String -> Program
simpleParse str = case parseJS str of
  Right p  -> p
  Left err -> error (show err)

parseExpr :: String -> Expr
parseExpr str = case jsParse (expr <* eof) NotStrict "" str of
  Right e  -> e
  Left err -> error (show err)

jsParse :: JSParser a -> Strictness -> SourceName -> String -> Either ParseError a
jsParse p strict sourceName text = runP p (initialParseState strict) sourceName text
