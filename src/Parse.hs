{-# LANGUAGE StandaloneDeriving, DeriveAnyClass #-}
module Parse ( parseJS
             , parseJS'
             , parseJS''
             , simpleParse
             , parseInFunction
             , parseExpr
             , ParseError) where

import Control.Applicative
import Text.Parsec hiding (optional)
import Data.Text (Text)
import qualified Data.Text as T
import Expr

import Parse.Types
import Parse.Lexical
import Parse.State
import Parse.Statements
import Parse.Number

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
