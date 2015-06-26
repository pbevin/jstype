module Parse.Lexical where

import Text.Parsec hiding (many, optional, (<|>), crlf)
import Control.Monad (replicateM, liftM, void, when, guard)
import Control.Applicative
import Data.List (nub, sortBy)
import Data.Char
import qualified Data.Map as M
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

trieFromList :: [String] -> Trie
trieFromList = foldr insertTrie emptyTrie





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
identifierName = lexeme $ (:) <$> unicode identifierStart <*> many (unicode identifierPart)

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

reverseLength :: String -> String -> Ordering
reverseLength a b = compare (length b) (length a)

lexeme :: JSParser a -> JSParser a
lexeme p = p <* whiteSpace

tok :: String -> JSParser String
tok = lexeme . string

keyword :: String -> JSParser ()
keyword = reserved

resOp :: JSParser String
resOp = go allOps <* whiteSpace
  where
    go :: Trie -> JSParser String
    go ops@(Trie isOp longer) = do
      ch <- lookAhead anyChar
      case (isOp, M.lookup ch longer) of
        (True,  Just t)  -> try (recurse ch t) <|> return ""
        (False, Just t)  -> recurse ch t
        (True,  Nothing) -> return ""
        (False, Nothing) -> fail ""
    recurse :: Char -> Trie -> JSParser String
    recurse ch t = anyChar >> (ch:) <$> go t

skip :: String -> JSParser ()
skip = void . tok

comma, semicolon :: JSParser ()
comma = skip ","
semicolon = skip ";"

plusminus :: JSParser String -> JSParser String
plusminus p = (:) <$> oneOf "+-" <*> p <|> p

number, decimal :: JSParser Double
number = read . snd <$> np
decimal = read . snd <$> decimalNumber True

numberText :: JSParser String
numberText = fst <$> np

np :: JSParser (String, String)
np = lexeme (octalNumber <|> hexNumber <|> decimalNumber False)
  where hexNumber = try $ do
          a <- char '0' >> oneOf "xX"
          b <- many1 hexDigit
          return $ dup $ '0':a:b

        octalNumber = ifNotStrict $ try $ do
          char '0' >> many1 octDigit >>= \oct -> return (oct, octToDec oct)

        octToDec :: String -> String
        octToDec = show . foldl (\x e -> 8*x + digitToInt e) 0

        dup x = (x,x)

decimalNumber :: Bool -> JSParser (String, String)
decimalNumber leadingZeros = lexeme (infinity <|> numberWithoutDecimal <|> numberWithDecimal)
  where decimalInt  = string "0" <|> positiveInt
        positiveInt = (:) <$> oneOf "123456789" <*> many digit
        decimalDigits = many1 digit

        fracPart = do
          dec <- char '.' >> optional (many1 digit)
          return $ ('.' :) `applyTo` (fromMaybe "" dec, fromMaybe "0" dec)

        expPart  = try $ (:) <$> oneOf "eE" <*> plusminus (many1 digit)

        numberWithoutDecimal = do
          b <- char '.' >> many1 digit
          c <- fromMaybe "" <$> optional expPart
          return ('.' : (b ++ c), "0." ++ (b ++ c))

        numberWithDecimal = do
          a <- if leadingZeros then decimalDigits else decimalInt
          mb <- optional fracPart
          c <- fromMaybe "" <$> optional expPart
          case mb of
            Nothing -> return $ dup (a ++ c)
            Just (b, b') -> return $ (\x -> (a ++ x ++ c)) `applyTo` (b, b')

        infinity = string "Infinity" >> return (dup "Infinity")

        applyTo = fmap
        dup x = (x,x)

-- ref 7.8.4
quotedString :: JSParser String
quotedString = mapToUtf16 <$>
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
  (a, b) <- lookAhead anyChar >>= escape
  return ('\\':a, b)

escape :: Char -> JSParser (String,String)
escape ch
  | isLineBreak ch = anyChar >>= \c -> return (['\\', c], "")
  | ch == '0' = try octalEscape <|> (char '0' >> lookAhead (noneOf "0123456789") >> return ("0", "\0"))
  | ch == 'u' = char 'u' >> replicateM 4 hexDigit >>= \s -> return ("u" ++ s, [hexToChar s])
  | ch `elem` "01234567" = octalEscape
  | otherwise = char ch >> return ([ch], [singleCharEscape ch])

singleCharEscape :: Char -> Char
singleCharEscape 'b' = '\b'
singleCharEscape 't' = '\t'
singleCharEscape 'n' = '\n'
singleCharEscape 'v' = '\v'
singleCharEscape 'f' = '\f'
singleCharEscape 'r' = '\r'
singleCharEscape ch  = ch

octalEscape :: JSParser (String,String)
octalEscape = failIfStrict >> choice [ oct1, oct2, oct3, oct4 ] >>= \d -> return (d, [octToChar d])
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

mapToUtf16 :: String -> String
mapToUtf16 = concatMap toUtf16
  where
    toUtf16 :: Char -> String
    toUtf16 ch
      | ord ch <= 0xFFFF = [ch]
      | otherwise        = [chr $ 0xD800 + a, chr $ 0xDC00 + b]
        where (a, b) = (ord ch - 0x10000) `divMod` 1024
