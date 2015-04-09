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

simpleParse :: String -> Expr
simpleParse str = case parseJS str of
  Right expr -> expr
  Left err   -> error (show err)


lexer = T.makeTokenParser javaStyle
parens = T.parens lexer
identifier = T.identifier lexer
integer = T.integer lexer
float = T.float lexer
reserved = T.reserved lexer
symbol = T.symbol lexer
whiteSpace = T.whiteSpace lexer

lexeme str = string str <* whiteSpace
resOp name = do
  string name
  notFollowedBy (oneOf "-+=&|^<>")
  whiteSpace
  return name


expr :: Parser Expr
expr = foldl (flip ($)) simple [
  postfixExpr,
  unaryExpr,
  binOps ["*", "/", "%"],
  binOps ["+", "-"],
  binOps [ ">>", ">>>", "<<" ],
  binOps [ ">=", "<=", ">", "<", "instanceof", "in" ],
  binOps [ "===", "!==", "==", "!=" ],
  binOps [ "&" ],
  binOps [ "^" ],
  binOps [ "|" ],
  binOps [ "&&" ],
  binOps [ "||" ],
  condExpr,
  assignExpr ]

assignExpr :: Parser Expr -> Parser Expr
assignExpr p = try(assign) <|> p
  where assign = do v <- identifier
                    op <- lexeme "=" <|> assignOp
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

unaryExpr :: Parser Expr -> Parser Expr
unaryExpr p = try(unop) <|> p
  where unop = do
          op <- choice $ map (try . resOp) $ unaryOps jsLang
          e <- p
          return $ UnOp op e

postfixExpr :: Parser Expr -> Parser Expr
postfixExpr p = try postfix <|> p
  where postfix = do
          e <- p
          op <- choice $ map (try . lexeme) $ postfixOps jsLang
          return $ PostOp op e

binOps :: [String] -> Parser Expr -> Parser Expr
binOps ops p = do
  e1 <- p
  choice $ [ bin p e1 op | op <- ops ] ++ [ return e1 ]

bin :: Parser Expr -> Expr -> String -> Parser Expr
bin p e1 op = do
  try (resOp op)
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
  where op name = try (lexeme name) >> return name











prop_showExpr expr = simpleParse (showExpr expr) == expr

