module Parse (parseJS, parseJS', simpleParse, parseExpr, prop_showExpr, prop_showProg, disprove, disprove') where

import Control.Applicative hiding (many, optional, (<|>))
import Test.QuickCheck
import Text.Parsec hiding (newline)
import Text.Parsec.String
import qualified Text.Parsec.Token as T
import Text.Parsec.Language (javaStyle)
import Data.List
import Data.Char
import ShowExpr
import Expr

import Debug.Trace

type ParseState = Bool -- "in" allowed
type JSParser = Parsec String ParseState

jsParse :: JSParser a -> SourceName -> String -> Either ParseError a
jsParse p = runP p True

parseJS :: String -> Either ParseError Program
parseJS str = parseJS' str ""

parseJS' :: String -> String -> Either ParseError Program
parseJS' str filename = jsParse (whiteSpace >> prog <* eof) filename str

simpleParse :: String -> Program
simpleParse str = case parseJS str of
  Right prog -> prog
  Left err   -> error (show err)

parseExpr :: String -> Expr
parseExpr str = case jsParse (expr <* eof) "" str of
  Right expr -> expr
  Left err   -> error (show err)


sameLine :: SourcePos -> SourcePos -> Bool
sameLine pos1 pos2 = sourceLine pos1 == sourceLine pos2

identStart  = (['a'..'z'] ++ ['A'..'Z'] ++ "$_")
identLetter = identStart ++ ['0'..'9']


javascript :: T.LanguageDef st
javascript = javaStyle
              { T.reservedNames = reservedWords jsLang
              , T.identStart = oneOf identStart
              , T.identLetter = oneOf identLetter
              , T.caseSensitive = True }

lexer = T.makeTokenParser javascript
parens = T.parens lexer
braces = T.braces lexer
brackets = T.brackets lexer
identifier = T.identifier lexer
integer = T.integer lexer
float = T.float lexer
reserved = T.reserved lexer
symbol = T.symbol lexer
whiteSpace = T.whiteSpace lexer

spaceNotNewline :: Char -> Bool
spaceNotNewline ch = isSpace ch && ch /= '\n'

noNewline :: JSParser ()
noNewline = satisfy spaceNotNewline >> return ()

allOps = sortBy reverseLength $ nub allJsOps
  where allJsOps = (assignOps jsLang) ++ (unaryOps jsLang) ++ (binaryOps jsLang) ++ (postfixOps jsLang)

reverseLength :: String -> String -> Ordering
reverseLength a b = compare (length b) (length a)

lexeme :: String -> JSParser String
lexeme str = string str <* whiteSpace

keyword :: String -> JSParser ()
keyword str = reserved str >> whiteSpace

resOp = do
  op <- choice (map (try . string) allOps)
  whiteSpace
  return op

comma = lexeme "," >> return ()
semicolon = lexeme ";" >> return ()


disableInKeyword, enableInKeyword :: JSParser ()
disableInKeyword = putState False
enableInKeyword  = putState True

removeIn :: [String] -> JSParser [String]
removeIn ops = do
  inKeywordEnabled <- getState
  return $ if inKeywordEnabled
           then ops
           else ops \\ ["in"]



prog :: JSParser Program
prog = Program <$> statementList

statementList :: JSParser [Statement]
statementList = many (statement)

terminated :: JSParser a -> JSParser a
terminated p = do
  pos1 <- getPosition
  result <- p
  pos2 <- getPosition

  if sameLine pos1 pos2
  then optional semicolon
  else return ()

  return result



statement :: JSParser Statement
statement = choice [ block <?> "block",
                     terminated exprStmt <?> "expression",
                     terminated varDecl <?> "var declaration",
                     ifStmt <?> "if",
                     forStmt <?> "for",
                     whileStmt <?> "while",
                     terminated doWhileStmt <?> "do...while",
                     terminated returnStmt <?> "return",
                     terminated breakStmt <?> "break",
                     terminated continueStmt <?> "continue",
                     terminated throwStmt <?> "throw",
                     terminated tryStmt <?> "try",
                     emptyStmt <?> ";",
                     debuggerStmt <?> "debugger" ]

block :: JSParser Statement
block = do
  stmts <- braces statementList
  return $ case stmts of
    [single] -> single
    _ -> Block stmts

realblock = Block <$> braces statementList

varDecl :: JSParser Statement
varDecl = (try $ keyword "var" >> VarDecl <$> varAssign `sepBy1` comma) <?> "variable declaration"

varAssign :: JSParser (String, Maybe Expr)
varAssign = do
  id <- identifier
  assignment id <|> return (id, Nothing)
    where assignment id = do
            lexeme "="
            e <- assignmentExpr
            return (id, Just e)

returnStmt :: JSParser Statement
returnStmt = do
  pos1 <- getPosition
  try (keyword "return")
  pos2 <- getPosition

  Return <$> if sameLine pos1 pos2
             then optionMaybe expr
             else pure Nothing

ifStmt :: JSParser Statement
ifStmt = do
  try $ keyword "if"
  test <- parens expr
  ifTrue <- statement
  ifFalse <- try elseClause <|> return Nothing
  return $ IfStatement test ifTrue ifFalse

elseClause :: JSParser (Maybe Statement)
elseClause = try (keyword "else" >> Just <$> statement)

whileStmt :: JSParser Statement
whileStmt = try $ keyword "while" >>
  WhileStatement <$> parens expr <*> statement

doWhileStmt :: JSParser Statement
doWhileStmt = try $ keyword "do" >> do
  stmt <- statement
  keyword "while"
  e <- parens expr
  return $ DoWhileStatement e stmt
  

forStmt :: JSParser Statement
forStmt = (try $ keyword "for") >>
  For <$> forHeader <*> statement

forHeader = parens $ (try forin <|> for3)
forin = ForIn <$> exprNoIn <*> (keyword "in" >> expr)
for3 = For3 <$> optionMaybe exprNoIn <*>
                (lexeme ";" >> optionMaybe expr) <*>
                (lexeme ";" >> optionMaybe expr)

exprStmt :: JSParser Statement
exprStmt = ExprStmt <$> expr

emptyStmt :: JSParser Statement
emptyStmt = semicolon >> return EmptyStatement

breakStmt :: JSParser Statement
breakStmt = keyword "break" >> return BreakStatement

continueStmt :: JSParser Statement
continueStmt = keyword "continue" >> return ContinueStatement

throwStmt :: JSParser Statement
throwStmt = try (keyword "throw") >> ThrowStatement <$> expr

tryStmt :: JSParser Statement
tryStmt = try (keyword "try") >> TryStatement <$> realblock
             <*> optionMaybe catch <*> optionMaybe finally
    where catch = try (keyword "catch") >>
                     Catch <$> parens identifier <*> realblock
          finally = try (keyword "finally") >>
                     Finally <$> realblock




debuggerStmt :: JSParser Statement
debuggerStmt = keyword "debugger" >> return DebuggerStatement











expr :: JSParser Expr
expr = assignmentExpr `chainl1` commaExpr
  where commaExpr = lexeme "," >> (return $ BinOp ",")

assignmentExpr :: JSParser Expr
assignmentExpr = foldr ($) simple [
  assignExpr,
  condExpr,
  binOps [ "||" ],
  binOps [ "&&" ],
  binOps [ "|" ],
  binOps [ "^" ],
  binOps [ "&" ],
  binOps [ "===", "!==", "==", "!=" ],
  binOps [ ">=", "<=", ">", "<", "instanceof", "in" ],
  binOps [ ">>>", ">>", "<<" ],
  binOps ["+", "-"],
  binOps ["*", "/", "%"],
  unaryExpr,
  postfixExpr,
  callExpr,
  memberExpr ] <?> "expr"

exprNoIn = disableInKeyword *> expr <* enableInKeyword

memberExpr :: JSParser Expr -> JSParser Expr
memberExpr p = (try (keyword "new") >> NewExpr <$> memberExpr p <*> parens argumentList)
           <|> baseMemberExpr p

baseMemberExpr :: JSParser Expr -> JSParser Expr
baseMemberExpr p = do
  base <- (functionExpr <|> p)
  extras <- many (dotExt <|> arrayExt)
  return $ foldl (flip ($)) base extras

dotExt :: JSParser (Expr -> Expr)
dotExt = try $ do
  char '.'
  id <- identifier
  return (\e -> MemberDot e id)

arrayExt :: JSParser (Expr -> Expr)
arrayExt = try $ do
  x <- brackets expr
  return (\a -> MemberGet a x)


functionExpr :: JSParser Expr
functionExpr = do
  try $ keyword "function"
  name <- optionMaybe identifier <?> "function name"
  params <- parens (identifier `sepBy` comma) <?> "parameter list"
  stmts <- braces statementList <?> "function body"
  return $ FunDef name params stmts

callExpr :: JSParser Expr -> JSParser Expr
callExpr p = do
  base <- p
  addons base
    where
      addons :: Expr -> JSParser Expr
      addons base = (parens argumentList >>= \args -> FunCall <$> addons base <*> pure args)
                <|> ((char '.' >> identifier) >>= \id -> MemberDot <$> addons base <*> pure id)
                <|> (brackets expr >>= \e -> MemberGet <$> addons base <*> pure e)
                <|> return base

argumentList :: JSParser [Expr]
argumentList = assignmentExpr `sepBy` comma

postfixExpr :: JSParser Expr -> JSParser Expr
postfixExpr p = do
  e <- p
  try (postfix e) <|> return e
    where postfix e = do
            op <- choice $ map (try . lexeme) $ postfixOps jsLang
            whiteSpace
            return $ PostOp op e

unaryExpr :: JSParser Expr -> JSParser Expr
unaryExpr p = (try(unop) <|> p) <?> "unary expr"
  where unop = do
          op <- choice $ map (try . string) $ sortBy reverseLength $ unaryOps jsLang
          whiteSpace
          e <- unaryExpr p
          return $ UnOp op e

binOps :: [String] -> JSParser Expr -> JSParser Expr
binOps ops p = do { ops' <- removeIn ops; p `chainl1` bin ops' }
  where bin ops = try $ do
          op <- resOp
          whiteSpace
          if op `elem` ops
          then return $ BinOp op
          else unexpected ("one of " ++ show ops)

condExpr :: JSParser Expr -> JSParser Expr
condExpr p = do
  test <- p
  choice [queryColon test, return test]
    where queryColon test = do
            lexeme "?"
            ifTrue <- expr
            lexeme ":"
            ifFalse <- expr
            return $ Cond test ifTrue ifFalse

assignExpr :: JSParser Expr -> JSParser Expr
assignExpr p = do
  expr <- p
  if isLHS expr
  then (assignment expr <|> return expr)
  else return expr

assignment lhs = do
  op <- lexeme "=" <|> assignOp
  rhs <- expr
  return $ Assign lhs op rhs

isLHS :: Expr -> Bool
isLHS e = True -- XXX

lhsExpr :: JSParser Expr
lhsExpr = memberExpr simple

simple :: JSParser Expr
simple = parens expr
     <|> arrayLiteral
     <|> objectLiteral
     <|> regexLiteral
     <|> this
     <|> var
     <|> num
     <|> str

this :: JSParser Expr
this = try $ keyword "this" >> return This

var :: JSParser Expr
var = identifier >>= return . ReadVar

str :: JSParser Expr
str = Str <$> quotedString

arrayLiteral :: JSParser Expr
arrayLiteral = ArrayLiteral <$> brackets (expr `sepBy` comma)

objectLiteral :: JSParser Expr
objectLiteral = ObjectLiteral <$> braces (propertyAssignment `sepBy` comma)
  where
    propertyAssignment = do
      name <- (IdentProp <$> identifier
                <|> StringProp <$> quotedString
                <|> NumProp <$> numericLiteral)
      lexeme ":"
      val <- assignmentExpr
      return (name, val)

regexLiteral :: JSParser Expr
regexLiteral = do
  try (char '/')
  first <- regexFirstChar
  rest <- many regexChar
  char '/'
  flags <- many (oneOf identLetter)
  whiteSpace

  return $ RegularExpression (first ++ concat rest) flags

regexFirstChar = (tostr $ noneOf "*\\/[\n")
             <|> regexBackslash
             <|> regexClass

regexChar      = (tostr $ noneOf "\\/\n")
             <|> regexBackslash
             <|> regexClass
regexBackslash = do
  b <- char '\\'
  c <- noneOf "\n"
  return [b,c]

regexClass = do
  xs <- brackets (many $ tostr (noneOf "\n\\]") <|> regexBackslash)
  return $ "[" ++ concat xs ++ "]"


tostr :: JSParser Char -> JSParser String
tostr p = do
  c <- p
  return [c]


quotedString :: JSParser String
quotedString = T.stringLiteral lexer <|> singleQuotedString

singleQuotedString :: JSParser String
singleQuotedString = do
  char '\''
  str <- many (noneOf "'")
  char '\''
  whiteSpace
  return str

num :: JSParser Expr
num = Num <$> numericLiteral

numericLiteral :: JSParser JSNum
numericLiteral = do
  val <- T.naturalOrFloat lexer
  return $ case val of
    Left int -> JSNum $ fromIntegral int
    Right dbl -> JSNum dbl


assignOp :: JSParser String
assignOp = choice $ map op $ assignOps jsLang
  where op name = try (lexeme name) >> return name











prop_showProg prog = counterexample (ppcode prog) $ simpleParse (ppcode prog) == prog
prop_showExpr expr = counterexample (ppcode expr) $ parseExpr (ppcode expr) == expr

disprove :: Program -> IO ()
disprove p = do
  putStrLn $ show p
  putStrLn $ ppcode p
  putStrLn $ show (simpleParse $ ppcode p)

disprove' :: Expr -> IO ()
disprove' e = do
  putStrLn $ show e
  putStrLn $ ppcode e
  putStrLn $ show (parseExpr $ ppcode e)
