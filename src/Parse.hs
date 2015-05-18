module Parse ( parseJS
             , parseJS'
             , simpleParse
             , parseExpr) where

import Control.Applicative
import Text.Parsec
import Expr

import Parse.Types
import Parse.Lexical
import Parse.State
import Parse.Statements

parseJS :: String -> Either ParseError Program
parseJS str = parseJS' str ""

parseJS' :: String -> String -> Either ParseError Program
parseJS' str filename = jsParse (whiteSpace >> prog <* eof) filename str

simpleParse :: String -> Program
simpleParse str = case parseJS str of
  Right p  -> p
  Left err -> error (show err)

parseExpr :: String -> Expr
parseExpr str = case jsParse (expr <* eof) "" str of
  Right e  -> e
  Left err -> error (show err)

jsParse :: JSParser a -> SourceName -> String -> Either ParseError a
jsParse p = runP p initialParseState
