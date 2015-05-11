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

parseJS :: String -> Either ParseError Program
-- parseJS str = parse (whiteSpace >> prog <* eof) "" str
parseJS str = parseJS' str ""

parseJS' :: String -> String -> Either ParseError Program
parseJS' str filename = parse (whiteSpace >> prog <* eof) filename str

simpleParse :: String -> Program
simpleParse str = case parseJS str of
  Right prog -> prog
  Left err   -> error (show err)

parseExpr :: String -> Expr
parseExpr str = case parse (expr <* eof) "" str of
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

noNewline :: Parser ()
noNewline = satisfy spaceNotNewline >> return ()

allOps = sortBy reverseLength $ nub allJsOps
  where allJsOps = (assignOps jsLang) ++ (unaryOps jsLang) ++ (binaryOps jsLang) ++ (postfixOps jsLang)

reverseLength :: String -> String -> Ordering
reverseLength a b = compare (length b) (length a)

lexeme str = string str <* whiteSpace
resOp = do
  op <- choice (map (try . string) allOps)
  whiteSpace
  return op

comma = lexeme "," >> return ()
semicolon = lexeme ";" >> return ()

prog :: Parser Program
prog = Program <$> statementList

statementList :: Parser [Statement]
statementList = many (statement)

terminated :: Parser a -> Parser a
terminated p = do
  pos1 <- getPosition
  result <- p
  pos2 <- getPosition

  if sameLine pos1 pos2
  then optional semicolon
  else return ()

  return result



statement :: Parser Statement
statement = choice [ block <?> "block",
                     terminated exprStmt <?> "expression",
                     terminated varDecl <?> "var declaration",
                     ifStmt <?> "if",
                     forStmt <?> "for",
                     whileStmt <?> "while",
                     terminated returnStmt <?> "return",
                     terminated breakStmt <?> "break",
                     terminated continueStmt <?> "continue",
                     terminated throwStmt <?> "throw",
                     terminated tryStmt <?> "try",
                     emptyStmt <?> ";",
                     debuggerStmt <?> "debugger" ]

block :: Parser Statement
block = do
  stmts <- braces statementList
  return $ case stmts of
    [single] -> single
    _ -> Block stmts

realblock = Block <$> braces statementList

varDecl :: Parser Statement
varDecl = (try $ lexeme "var" >> VarDecl <$> varAssign `sepBy1` comma) <?> "variable declaration"

varAssign :: Parser (String, Maybe Expr)
varAssign = do
  id <- identifier
  assignment id <|> return (id, Nothing)
    where assignment id = do
            lexeme "="
            e <- expr
            return (id, Just e)

returnStmt :: Parser Statement
returnStmt = do
  pos1 <- getPosition
  try (reserved "return")
  pos2 <- getPosition

  Return <$> if sameLine pos1 pos2
             then optionMaybe expr
             else pure Nothing

ifStmt :: Parser Statement
ifStmt = do
  try $ lexeme "if"
  test <- parens expr
  ifTrue <- statement
  ifFalse <- try elseClause <|> return Nothing
  return $ IfStatement test ifTrue ifFalse

elseClause :: Parser (Maybe Statement)
elseClause = try (lexeme "else" >> Just <$> statement)

whileStmt :: Parser Statement
whileStmt = try $ lexeme "while" >>
  WhileStatement <$> parens expr <*> statement

forStmt :: Parser Statement
forStmt = (try $ lexeme "for") >>
  For <$> forHeader <*> statement

forHeader = parens $ (try forin <|> for3)
forin = ForIn <$> expr <*> (lexeme "in" >> expr)
for3 = For3 <$> optionMaybe expr <*>
                (lexeme ";" >> optionMaybe expr) <*>
                (lexeme ";" >> optionMaybe expr)

exprStmt :: Parser Statement
exprStmt = ExprStmt <$> expr

emptyStmt :: Parser Statement
emptyStmt = semicolon >> return EmptyStatement

breakStmt :: Parser Statement
breakStmt = lexeme "break" >> return BreakStatement

continueStmt :: Parser Statement
continueStmt = lexeme "continue" >> return ContinueStatement

throwStmt :: Parser Statement
throwStmt = try (lexeme "throw") >> ThrowStatement <$> expr

tryStmt :: Parser Statement
tryStmt = try (lexeme "try") >> TryStatement <$> realblock
             <*> optionMaybe catch <*> optionMaybe finally
    where catch = try (lexeme "catch") >>
                     Catch <$> parens identifier <*> realblock
          finally = try (lexeme "finally") >>
                     Finally <$> realblock




debuggerStmt :: Parser Statement
debuggerStmt = lexeme "debugger" >> return DebuggerStatement











expr :: Parser Expr
expr = foldr ($) simple [
  assignExpr,
  condExpr,
  binOps [ "||" ],
  binOps [ "&&" ],
  binOps [ "|" ],
  binOps [ "^" ],
  binOps [ "&" ],
  binOps [ "===", "!==", "==", "!=" ],
  binOps [ ">=", "<=", ">", "<", "instanceof" ],
  binOps [ ">>>", ">>", "<<" ],
  binOps ["+", "-"],
  binOps ["*", "/", "%"],
  unaryExpr,
  postfixExpr,
  callExpr,
  memberExpr ] <?> "expr"

memberExpr :: Parser Expr -> Parser Expr
memberExpr p = (try (lexeme "new") >> NewExpr <$> memberExpr p <*> parens argumentList)
           <|> baseMemberExpr p

baseMemberExpr :: Parser Expr -> Parser Expr
baseMemberExpr p = do
  base <- (functionExpr <|> p)
  extras <- many (dotExt <|> arrayExt)
  return $ foldl (flip ($)) base extras

dotExt :: Parser (Expr -> Expr)
dotExt = try $ do
  char '.'
  id <- identifier
  return (\e -> MemberDot e id)

arrayExt :: Parser (Expr -> Expr)
arrayExt = try $ do
  x <- brackets expr
  return (\a -> MemberGet a x)


functionExpr :: Parser Expr
functionExpr = do
  try $ lexeme "function"
  name <- optionMaybe identifier <?> "function name"
  params <- parens (identifier `sepBy` comma) <?> "parameter list"
  stmts <- braces statementList <?> "function body"
  return $ FunDef name params stmts

callExpr :: Parser Expr -> Parser Expr
callExpr p = do
  base <- p
  addons base
    where
      addons :: Expr -> Parser Expr
      addons base = (parens argumentList >>= \args -> FunCall <$> addons base <*> pure args)
                <|> ((char '.' >> identifier) >>= \id -> MemberDot <$> addons base <*> pure id)
                <|> (brackets expr >>= \e -> MemberGet <$> addons base <*> pure e)
                <|> return base

argumentList :: Parser [Expr]
argumentList = expr `sepBy` comma

postfixExpr :: Parser Expr -> Parser Expr
postfixExpr p = do
  e <- p
  try (postfix e) <|> return e
    where postfix e = do
            op <- choice $ map (try . lexeme) $ postfixOps jsLang
            whiteSpace
            return $ PostOp op e

unaryExpr :: Parser Expr -> Parser Expr
unaryExpr p = (try(unop) <|> p) <?> "unary expr"
  where unop = do
          op <- choice $ map (try . string) $ sortBy reverseLength $ unaryOps jsLang
          whiteSpace
          e <- p
          return $ UnOp op e

binOps :: [String] -> Parser Expr -> Parser Expr
binOps ops p = p `chainl1` bin ops
  where bin ops = try $ do
          op <- resOp
          whiteSpace
          if op `elem` ops
          then return $ BinOp op
          else unexpected ("one of " ++ show ops)

condExpr :: Parser Expr -> Parser Expr
condExpr p = do
  test <- p
  choice [queryColon test, return test]
    where queryColon test = do
            lexeme "?"
            ifTrue <- expr
            lexeme ":"
            ifFalse <- expr
            return $ Cond test ifTrue ifFalse

assignExpr :: Parser Expr -> Parser Expr
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

lhsExpr :: Parser Expr
lhsExpr = memberExpr simple

simple :: Parser Expr
simple = parens expr
     <|> arrayLiteral
     <|> objectLiteral
     <|> regexLiteral
     <|> this
     <|> var
     <|> num
     <|> str

this :: Parser Expr
this = try $ lexeme "this" >> return This

var :: Parser Expr
var = identifier >>= return . ReadVar

str :: Parser Expr
str = Str <$> quotedString

arrayLiteral :: Parser Expr
arrayLiteral = ArrayLiteral <$> brackets (expr `sepBy` comma)

objectLiteral :: Parser Expr
objectLiteral = ObjectLiteral <$> braces (propertyAssignment `sepBy` comma)

regexLiteral :: Parser Expr
regexLiteral = do
  try (char '/')
  first <- regexFirstChar
  rest <- many regexChar
  char '/'
  flags <- many (oneOf identLetter)

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


tostr :: Parser Char -> Parser String
tostr p = do
  c <- p
  return [c]

propertyAssignment :: Parser (PropertyName, Expr)
propertyAssignment = do
  name <- (IdentProp <$> identifier
            <|> StringProp <$> quotedString
            <|> NumProp <$> numericLiteral)
  lexeme ":"
  val <- expr
  return (name, val)


quotedString :: Parser String
quotedString = T.stringLiteral lexer <|> singleQuotedString

singleQuotedString :: Parser String
singleQuotedString = do
  char '\''
  str <- many (noneOf "'")
  char '\''
  whiteSpace
  return str

num :: Parser Expr
num = Num <$> numericLiteral

numericLiteral :: Parser JSNum
numericLiteral = do
  val <- T.naturalOrFloat lexer
  return $ case val of
    Left int -> JSNum $ fromIntegral int
    Right dbl -> JSNum dbl


assignOp :: Parser String
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
