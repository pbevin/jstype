module Parse.Lexical where

import Text.Parsec hiding (many, optional, (<|>), crlf)
import Control.Monad (replicateM, liftM, void, when, guard)
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

identifierName :: JSParser Ident
identifierName = lexeme $ (:) <$> identifierStart <*> many (identifierPart)
  where
    identifierStart = oneOf identStart <|> (char '\\' >> char 'u' >> unicodeIdentifierEscape)
    identifierPart = oneOf identLetter <|> (char '\\' >> char 'u' >> unicodeIdentifierEscape)

identifier :: JSParser Ident
identifier = try $ do
  name <- identifierName
  illegal <- currentReservedWords
  if name `elem` illegal
  then unexpected $ "reserved word \"" ++ name ++ "\""
  else return name

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
comment = void parseComment
  where parseComment = (try lineComment <|> try blockComment) <?> "comment"
        lineComment = string "//" >> manyTill anyChar (try lineBreak)
        blockComment = string "/*" >> manyTill anyChar (try $ string "*/")

whiteSpace :: JSParser ()
whiteSpace = void $ many (lineBreak <|> void (satisfy isSpace) <|> comment)

linebreaks :: String
linebreaks = [ '\n', '\r', '\x2028', '\x2029' ]

isLineBreak :: Char -> Bool
isLineBreak ch = ch `elem` linebreaks

lineBreak :: JSParser ()
lineBreak = let crlf = try (string "\r\n") <?> "newline"
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
  where decimal  = many1 digit
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

-- ref 7.8.4
quotedString :: JSParser String
quotedString =
  ifInDirectivePrologue (doubleQuotedString fst <|> singleQuotedString fst)
                        (doubleQuotedString snd <|> singleQuotedString snd)


doubleQuotedString :: ((String,String) -> String) -> JSParser String
doubleQuotedString convert = do
  char '"'
  str <- concat . map convert <$> many (normalChar "\"\\" <|> escapeSequence)
  char '"'
  whiteSpace
  return str

singleQuotedString :: ((String,String) -> String) -> JSParser String
singleQuotedString convert = do
  char '\''
  str <- concat . map convert <$> many (normalChar "\'\\" <|> escapeSequence)
  char '\''
  whiteSpace
  return str

normalChar :: String -> JSParser (String, String)
normalChar unwanted = do
  ch <- noneOf (unwanted ++ linebreaks)
  return ([ch], [ch])

escapeSequence :: JSParser (String, String)
escapeSequence = do
  char '\\'
  (a, b) <- anyChar >>= escape
  return ('\\':a, b)

escape :: Char -> JSParser (String,String)
escape '0' = lookAhead (noneOf "0123456789") >> return ("0", "\0")
escape 'u' = replicateM 4 hexDigit >>= \s -> return ("u" ++ s, [hexToChar s])
escape ch  = return ([ch], [singleCharEscape ch])

singleCharEscape :: Char -> Char
singleCharEscape 'b' = '\b'
singleCharEscape 't' = '\t'
singleCharEscape 'n' = '\n'
singleCharEscape 'v' = '\v'
singleCharEscape 'f' = '\f'
singleCharEscape 'r' = '\r'
singleCharEscape ch  = ch

unicodeEscape :: JSParser Char
unicodeEscape = liftM hexToChar $ replicateM 4 hexDigit

hexToChar :: String -> Char
hexToChar = chr . fst . head . readHex

unicodeIdentifierEscape :: JSParser Char
unicodeIdentifierEscape = do
  ch <- unicodeEscape
  guard $ ch `elem` identLetter
  return ch
