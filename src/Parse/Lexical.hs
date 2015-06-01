module Parse.Lexical where

import Text.Parsec hiding (many, optional, (<|>), crlf)
import Control.Monad (void, when)
import Control.Applicative
import Data.List (nub, sortBy)
import Data.Char (isSpace, chr)
import Numeric (readHex)
import Parse.Types
import Data.Maybe
import Expr
import Parse.State

import Debug.Trace

sameLine :: SourcePos -> SourcePos -> Bool
sameLine pos1 pos2 = sourceLine pos1 == sourceLine pos2

identStart, identLetter :: String
identStart  = ['a'..'z'] ++ ['A'..'Z'] ++ "$_"
identLetter = identStart ++ ['0'..'9']

surround :: String -> String -> JSParser a -> JSParser a
surround lhs rhs p = tok lhs *> p <* tok rhs

parens, braces, brackets :: JSParser a -> JSParser a
parens = surround "(" ")"
braces = surround "{" "}"
brackets = surround "[" "]"


identifier :: JSParser String
identifier = try $ do
  name <- ident
  illegal <- currentReservedWords
  if name `elem` illegal
  then unexpected $ "reserved word " ++ name
  else whiteSpace >> return name
  where ident = (:) <$> oneOf identStart <*> many (oneOf identLetter)

currentReservedWords :: JSParser [String]
currentReservedWords = do
  strict <- getStrictness
  if strict == NotStrict
  then return $ reservedWords jsLang
  else return $ reservedWords jsLang ++ reservedWordsStrict jsLang

reserved :: String -> JSParser ()
reserved word = void $ try $ do
  string word
  notFollowedBy (oneOf identLetter) <?> "end of " ++ word
  whiteSpace

comment :: JSParser ()
comment = void $ (try lineComment <|> try blockComment)
  where lineComment = string "//" >> manyTill anyChar (try lineBreak)
        blockComment = string "/*" >> manyTill anyChar (try $ string "*/")

whiteSpace :: JSParser ()
whiteSpace = void $ many (lineBreak <|> void (satisfy isSpace) <|> comment)

isLineBreak :: Char -> Bool
isLineBreak ch = ch == '\n' || ch == '\r' || ch == '\x2028' || ch == '\x2029'

lineBreak :: JSParser ()
lineBreak = let crlf = try (string "\r\n")
                breakChar = satisfy isLineBreak >>= \ch ->
                  when (ch /= '\n') $ do
                    pos <- getPosition
                    setPosition $ nextLine pos
            in (void crlf <|> void breakChar)

nextLine :: SourcePos -> SourcePos
nextLine pos = incSourceLine (setSourceColumn pos 1) 1

allOps :: [String]
allOps = sortBy reverseLength $ nub allJsOps
  where allJsOps = assignOps jsLang ++ unaryOps jsLang ++ binaryOps jsLang ++ postfixOps jsLang

reverseLength :: String -> String -> Ordering
reverseLength a b = compare (length b) (length a)

lexeme :: JSParser a -> JSParser a
lexeme p = p <* whiteSpace

tok :: String -> JSParser String
tok = lexeme . string

keyword :: String -> JSParser ()
keyword = reserved

resOp :: JSParser String
resOp = do
  op <- choice (map (try . string) allOps)
  whiteSpace
  return op

skip :: String -> JSParser ()
skip = void . tok

comma, semicolon :: JSParser ()
comma = skip ","
semicolon = skip ";"

plusminus :: JSParser String -> JSParser String
plusminus p = (:) <$> oneOf "+-" <*> p <|> p

number :: JSParser Double
number = read . snd <$> np

numberText :: JSParser String
numberText = fst <$> np

np :: JSParser (String, String)
np = lexeme (hexNumber <|> numberWithoutDecimal <|> numberWithDecimal)
  where decimal  = dup $ many1 digit
        fracPart = do
          dec <- char '.' >> optional decimal
          return $ ('.' :) `applyTo` (fromMaybe "" dec, fromMaybe "0" dec)

        expPart  = (:) <$> oneOf "eE" <*> plusminus decimal

        numberWithoutDecimal = do
          b <- char '.' >> decimal
          c <- fromMaybe "" <$> optional expPart
          return ('.' : (b ++ c), "0." ++ (b ++ c))

        numberWithDecimal = do
          a <- decimal
          mb <- optional fracPart
          c <- fromMaybe "" <$> optional expPart
          case mb of
            Nothing -> return $ dup (a ++ c)
            Just (b, b') -> return $ (\x -> (a ++ x ++ c)) `applyTo` (b, b')

        hexNumber = do
          a <- try $ (char '0' >> oneOf "xX")
          b <- many hexDigit
          return $ dup $ '0':a:b

        dup x = (x,x)
        applyTo = fmap
