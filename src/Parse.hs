module Parse (parseJS, simpleParse, parseExpr, prop_showExpr, prop_showProg) where

import Control.Applicative hiding (many, optional, (<|>))
import Test.QuickCheck
import Text.Parsec hiding (newline)
import Text.Parsec.String
import qualified Text.Parsec.Token as T
import Text.Parsec.Language (javaStyle)
import Data.List
import ShowExpr
import Expr

import Debug.Trace

parseJS :: String -> Either ParseError Program
parseJS str = parse (whiteSpace >> prog <* eof) "" str

simpleParse :: String -> Program
simpleParse str = case parseJS str of
  Right prog -> prog
  Left err   -> error (show err)

parseExpr :: String -> Expr
parseExpr str = case parse (expr <* eof) "" str of
  Right expr -> expr
  Left err   -> error (show err)



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

newline = whiteSpace >> char '\n' >> whiteSpace

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
statementList = many (statement <* optional terminator)

terminator = semicolon
-- terminator = choice [ semicolon, newline ]

statement :: Parser Statement
statement = choice [ block <?> "block",
                     varDecl <?> "var declaration",
                     whileStmt <?> "while",
                     returnStmt <?> "return",
                     exprStmt <?> "expression",
                     debuggerStmt ]

block :: Parser Statement
block = Block <$> braces (statementList)

varDecl :: Parser Statement
varDecl = try $ lexeme "var" >> VarDecl <$> varAssign `sepBy1` comma

varAssign :: Parser (String, Maybe Expr)
varAssign = do
  id <- identifier
  try (assignment id) <|> return (id, Nothing)
    where assignment id = do
            lexeme "="
            e <- expr
            return (id, Just e)

returnStmt :: Parser Statement
returnStmt = try $ lexeme "return" >> ReturnStatement <$> expr

whileStmt :: Parser Statement
whileStmt = try $ lexeme "while" >>
  WhileStatement <$> parens expr <*> statement

exprStmt :: Parser Statement
exprStmt = ExpressionStatement <$> expr

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
  binOps [ ">=", "<=", ">", "<", "instanceof", "in" ],
  binOps [ ">>>", ">>", "<<" ],
  binOps ["+", "-"],
  binOps ["*", "/", "%"],
  unaryExpr,
  postfixExpr,
  callExpr,
  memberExpr ]

memberExpr :: Parser Expr -> Parser Expr
memberExpr p = do
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
  fun <- p
  applications <- many (parens argumentList)
  return $ foldl FunCall fun applications

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
unaryExpr p = try(unop) <|> p
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
assignExpr p = try(lookAhead identifier >> assign) <|> p
  where assign = do lhs <- lhsExpr
                    op <- lexeme "=" <|> assignOp
                    rhs <- expr
                    return $ Assign lhs op rhs

lhsExpr :: Parser Expr
lhsExpr = memberExpr simple

simple :: Parser Expr
simple = parens expr <|> var <|> num <|> str

var :: Parser Expr
var = identifier >>= return . ReadVar

str :: Parser Expr
str = (T.stringLiteral lexer <|> singleQuotedString) >>= return . Str

singleQuotedString :: Parser String
singleQuotedString = do
  char '\''
  str <- many (noneOf "'")
  char '\''
  whiteSpace
  return str

num :: Parser Expr
num = do
  val <- T.naturalOrFloat lexer
  case val of
    Left int -> return $ Num $ JSNum $ fromIntegral int
    Right dbl -> return $ Num $ JSNum dbl


assignOp :: Parser String
assignOp = choice $ map op $ assignOps jsLang
  where op name = try (lexeme name) >> return name











prop_showProg prog = simpleParse (showProg prog) == prog
prop_showExpr expr = parseExpr (showExpr expr) == expr
