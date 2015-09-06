{-# LANGUAGE OverloadedStrings #-}

module Parse.Lexical where

import Text.Parsec hiding (many, optional, (<|>), crlf)
import Control.Monad (replicateM, liftM, void, when, guard)
import Control.Applicative
import Data.List (nub, sortBy)
import Data.Char
import qualified Data.Map as M
import qualified Data.Text as T
import Data.Text (Text)
import Data.Monoid
import Numeric (readHex, readOct)
import Parse.Types
import Data.Maybe
import Expr
import Unicode
import Parse.State

import Debug.Trace


data Trie = Trie Bool (M.Map Char Trie) deriving (Show, Eq)

emptyTrie :: Trie
emptyTrie = Trie False M.empty

insertTrie :: String -> Trie -> Trie
insertTrie []     (Trie _    m) = Trie True m
insertTrie (x:xs) (Trie isOp m) =
  Trie isOp $ M.alter (Just . maybe (trieFromString xs) (insertTrie xs)) x m

trieFromString :: String -> Trie
trieFromString = foldr addToTrie (Trie True M.empty)
  where
    addToTrie x trie = Trie False (M.singleton x trie)

trieFromList :: [Text] -> Trie
trieFromList = foldr insertTrie emptyTrie . map T.unpack


sameLine :: SourcePos -> SourcePos -> Bool
sameLine pos1 pos2 = sourceLine pos1 == sourceLine pos2

identStart, identLetter :: String
identStart  = ['a'..'z'] ++ ['A'..'Z'] ++ "$_"
identLetter = identStart ++ ['0'..'9']

surround :: Text -> Text -> JSParser a -> JSParser a
surround lhs rhs p = tok lhs *> p <* tok rhs

parens, braces, brackets :: JSParser a -> JSParser a
parens = surround "(" ")"
braces = surround "{" "}"
brackets = surround "[" "]"

unicode :: (Char -> Bool) -> JSParser Char
unicode p = satisfy p <|> escaped
  where
    escaped = do
      char '\\'
      char 'u'
      ch <- unicodeEscape
      guard (p ch)
      return ch

identifierStart, identifierPart :: Char -> Bool
identifierStart ch = ch == '$' || ch == '_' || isLetter ch
identifierPart  ch = identifierStart ch || isDigit ch

identifierName :: JSParser Ident
identifierName = lexeme $ do
  x <- unicode identifierStart
  xs <- many (unicode identifierPart)
  return . T.pack $ x:xs

identifier :: JSParser Ident
identifier = try $ do
  name <- identifierName
  illegal <- currentReservedWords
  if name `elem` illegal
  then unexpected . T.unpack $ "reserved word \"" <> name <> "\""
  else return name

currentReservedWords :: JSParser [Text]
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
        lineComment = string "//" >> manyTill anyChar (try lineBreak <|> eof)
        blockComment = string "/*" >> manyTill anyChar (try $ string "*/")

whiteSpace :: JSParser ()
whiteSpace = void $ many (lineBreak <|> void (satisfy isJsSpace) <|> comment)

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

allOps :: Trie
allOps = trieFromList allJsOps
  where allJsOps = assignOps jsLang ++ unaryOps jsLang ++ binaryOps jsLang ++ postfixOps jsLang

lexeme :: JSParser a -> JSParser a
lexeme p = p <* whiteSpace

tok :: Text -> JSParser Text
tok str = lexeme (text str)

text :: Text -> JSParser Text
text t = T.pack <$> string (T.unpack t)

keyword :: String -> JSParser ()
keyword = reserved

resOp :: JSParser Text
resOp = go allOps <* whiteSpace
  where
    go :: Trie -> JSParser Text
    go ops@(Trie isOp longer) = do
      ch <- lookAhead anyChar
      case (isOp, M.lookup ch longer) of
        (True,  Just t)  -> try (recurse ch t) <|> return ""
        (False, Just t)  -> recurse ch t
        (True,  Nothing) -> return ""
        (False, Nothing) -> fail ""
    recurse :: Char -> Trie -> JSParser Text
    recurse ch t = anyChar >> (T.cons ch) <$> go t

skip :: Text -> JSParser ()
skip = void . tok

comma, semicolon :: JSParser ()
comma = skip ","
semicolon = skip ";"

-- ref 7.8.4
quotedString :: JSParser Text
quotedString = mapToUtf16 <$>
  ifInDirectivePrologue (doubleQuotedString fst <|> singleQuotedString fst)
                        (doubleQuotedString snd <|> singleQuotedString snd)


doubleQuotedString :: ((Text,Text) -> Text) -> JSParser Text
doubleQuotedString convert = do
  char '"'
  str <- T.concat . map convert <$> many (normalChar "\"\\" <|> escapeSequence)
  char '"'
  whiteSpace
  return str

singleQuotedString :: ((Text,Text) -> Text) -> JSParser Text
singleQuotedString convert = do
  char '\''
  str <- T.concat . map convert <$> many (normalChar "\'\\" <|> escapeSequence)
  char '\''
  whiteSpace
  return str

normalChar :: Text -> JSParser (Text, Text)
normalChar unwanted = do
  ch <- noneOf (T.unpack unwanted ++ linebreaks)
  return (T.pack [ch], T.pack [ch])

escapeSequence :: JSParser (Text, Text)
escapeSequence = do
  char '\\'
  (a, b) <- lookAhead anyChar >>= escape
  return ('\\' `T.cons` a, b)

escape :: Char -> JSParser (Text,Text)
escape ch
  | isLineBreak ch = anyChar >>= \c -> return (T.pack $ ['\\', c], "")
  | ch == '0' = try octalEscape <|> (char '0' >> lookAhead (noneOf "0123456789") >> return ("0", "\0"))
  | ch == 'u' = char 'u' >> replicateM 4 hexDigit >>= \s -> return (T.pack $ "u" ++ s, T.pack [hexToChar s])
  | ch == 'x' = char 'x' >> replicateM 2 hexDigit >>= \s -> return (T.pack $ "x" ++ s, T.pack [hexToChar s])
  | ch `elem` ("01234567" :: [Char]) = octalEscape
  | otherwise = char ch >> return (T.pack [ch], T.pack [singleCharEscape ch])

singleCharEscape :: Char -> Char
singleCharEscape 'b' = '\b'
singleCharEscape 't' = '\t'
singleCharEscape 'n' = '\n'
singleCharEscape 'v' = '\v'
singleCharEscape 'f' = '\f'
singleCharEscape 'r' = '\r'
singleCharEscape ch  = ch

octalEscape :: JSParser (Text,Text)
octalEscape = failIfStrict >> choice [ oct1, oct2, oct3, oct4 ] >>= \d -> return (T.pack d, T.pack [octToChar d])
  where
    oct1 = try $ join1 <$> oneOf "1234567" <* lookAhead (noneOf "0123456789")
    oct2 = try $ join2 <$> oneOf "123" <*> oneOf "01234567" <* lookAhead (noneOf "0123456789")
    oct3 = try $ join2 <$> oneOf "4567" <*> oneOf "01234567"
    oct4 = try $ join3 <$> oneOf "123" <*> oneOf "01234567" <*> oneOf "01234567"
    join1 a     = [a]
    join2 a b   = [a,b]
    join3 a b c = [a,b,c]

unicodeEscape :: JSParser Char
unicodeEscape = liftM hexToChar $ replicateM 4 hexDigit

hexToChar :: String -> Char
hexToChar = chr . fst . head . readHex

octToChar :: String -> Char
octToChar = chr . fst . head . readOct

mapToUtf16 :: Text -> Text
mapToUtf16 = T.concatMap toUtf16
  where
    toUtf16 :: Char -> Text
    toUtf16 ch
      | ord ch <= 0xFFFF = T.pack [ch]
      | otherwise        = T.pack [chr $ 0xD800 + a, chr $ 0xDC00 + b]
        where (a, b) = (ord ch - 0x10000) `divMod` 1024



xxnum :: JSParser (Either Integer Double)
xxnum = return (Left 0)
