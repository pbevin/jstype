module Parse (parseJS, simpleParse, prop_showExpr, allOps) where

import Control.Applicative hiding (many, optional, (<|>))
import Test.QuickCheck
import Text.Parsec
import Text.Parsec.String
import qualified Text.Parsec.Token as T
import Text.Parsec.Language (javaStyle)
import Data.List
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
braces = T.braces lexer
brackets = T.brackets lexer
identifier = T.identifier lexer
integer = T.integer lexer
float = T.float lexer
reserved = T.reserved lexer
symbol = T.symbol lexer
whiteSpace = T.whiteSpace lexer

allOps = sortBy reverseLength $ nub allJsOps
  where allJsOps = (assignOps jsLang) ++ (unaryOps jsLang) ++ (binaryOps jsLang) ++ (postfixOps jsLang)

reverseLength :: String -> String -> Ordering
reverseLength a b = compare (length b) (length a)

lexeme str = string str <* whiteSpace
resOp = do
  op <- choice (map (try . string) allOps)
  whiteSpace
  return op


expr :: Parser Expr
expr = foldl (flip ($)) simple [
  memberExpr,
  callExpr,
  postfixExpr,
  unaryExpr,
  binOps ["*", "/", "%"],
  binOps ["+", "-"],
  binOps [ ">>>", ">>", "<<" ],
  binOps [ ">=", "<=", ">", "<", "instanceof", "in" ],
  binOps [ "===", "!==", "==", "!=" ],
  binOps [ "&" ],
  binOps [ "^" ],
  binOps [ "|" ],
  binOps [ "&&" ],
  binOps [ "||" ],
  condExpr,
  assignExpr ]

memberExpr :: Parser Expr -> Parser Expr
memberExpr p = do
  try functionExpr <|> p

functionExpr :: Parser Expr
functionExpr = do
  lexeme "function"
  return () <?> "function name"
  parens (return () <?> "parameter list")
  braces (return () <?> "function body")
  return $ FunDef Nothing [] []

callExpr :: Parser Expr -> Parser Expr
callExpr p = do
  fun <- p
  applications <- many (parens argumentList)
  return $ foldl FunCall fun applications

argumentList :: Parser [Expr]
argumentList = expr `sepBy` (lexeme ",")

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
binOps ops p = do
  e1 <- p
  try (binop e1) <|> return e1
    where binop e1 = do
            op <- resOp
            whiteSpace
            if op `elem` ops
              then p >>= return . BinOp op e1
              else fail "no"

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
assignExpr p = try(assign) <|> p
  where assign = do v <- identifier
                    op <- lexeme "=" <|> assignOp
                    e <- expr
                    return $ Assign v op e

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

