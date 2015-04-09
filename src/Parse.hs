module Parse (parseJS, simpleParse, prop_showExpr) where

import Control.Applicative hiding (many, optional, (<|>))
import Test.QuickCheck
import Text.Parsec
import Text.Parsec.String
import qualified Text.Parsec.Token as T
import Text.Parsec.Language (javaStyle)
import Expr

import Debug.Trace

parseJS :: String -> Either ParseError Expr
parseJS str = parse (expr <* eof) "" str

simpleParse str = case parseJS str of
  Right expr -> expr
  Left err   -> error (show err)


funStyle :: T.LanguageDef st
funStyle = javaStyle

lexer = T.makeTokenParser funStyle
parens = T.parens lexer
identifier = T.identifier lexer
integer = T.integer lexer
float = T.float lexer
reserved = T.reserved lexer
symbol = T.symbol lexer

lexeme str = string str <* T.whiteSpace lexer


-- binary name = Infix (do { lexeme name; return (BinOp name) }) AssocLeft
-- unary name = Prefix (do { lexeme name; return (UnOp name) })

-- table = [
--   [ unary "-", unary "+" ],
--   [ unary "!" ],
--   [ binary "&", binary "|" ],
--   [ binary "*", binary "/" ],
--   [ binary "+", binary "-" ],
--   [ binary "==" ] ]

expr :: Parser Expr
expr = assignExpr $ condExpr $ additionExpr $ multiplicationExpr $ unary $ simple

assignExpr :: Parser Expr -> Parser Expr
assignExpr p = try(assign) <|> p
  where assign = do v <- identifier
                    op <- assignOp
                    e <- expr
                    return $ Assign v op e

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

additionExpr :: Parser Expr -> Parser Expr
additionExpr p = do
  e1 <- p
  choice [bin p e1 "+", bin p e1 "-", return e1]

multiplicationExpr :: Parser Expr -> Parser Expr
multiplicationExpr p = do
  e1 <- p
  choice [bin p e1 "*", bin p e1 "/", return e1]

unary :: Parser Expr -> Parser Expr
unary p = try(unop) <|> p
  where unop = do
          op <- choice [lexeme "-", lexeme "+", lexeme "!"]
          e <- p
          return $ UnOp op e

bin :: Parser Expr -> Expr -> String -> Parser Expr
bin p e1 op = do
  lexeme op
  notFollowedBy $ char '='
  e2 <- p
  return $ BinOp op e1 e2

simple :: Parser Expr
simple = parens expr <|> var <|> num <|> str

var :: Parser Expr
var = identifier >>= return . ReadVar

str :: Parser Expr
str = T.stringLiteral lexer >>= return . Str

num :: Parser Expr
num = do
  val <- T.naturalOrFloat lexer
  case val of
    Left int -> return $ Num $ JSNum $ fromIntegral int
    Right dbl -> return $ Num $ JSNum dbl


assignOp :: Parser String
assignOp = choice $ map op $ assignOps jsLang
  where op name = lexeme name >> return name














prop_showExpr expr = simpleParse (showExpr expr) == expr

