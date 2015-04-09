module Parse where

import Control.Applicative hiding (many, optional, (<|>))
import Test.QuickCheck
import Text.Parsec
import Text.Parsec.String
import Text.Parsec.Expr
import qualified Text.Parsec.Token as T
import Text.Parsec.Language (javaStyle)
import Expr


funStyle :: T.LanguageDef st
funStyle = javaStyle

lexer = T.makeTokenParser funStyle
parens = T.parens lexer
identifier = T.identifier lexer
integer = T.integer lexer
float = T.float lexer
reserved = T.reserved lexer
reservedOp = T.reservedOp lexer
symbol = T.symbol lexer




mkUnOp op a@(Num (JSNum n)) = case op of
  "-" -> Num $ JSNum $ -n
  "+" -> Num $ JSNum n
  _ -> UnOp op a
mkUnOp op a = UnOp op a
mkBinOp op a b = BinOp op a b

binary name = Infix (do { reservedOp name; return (mkBinOp name) }) AssocLeft
unary name = Prefix (do { reservedOp name; return (mkUnOp name) })

table = [
  [ unary "-", unary "+" ],
  [ unary "!" ],
  [ binary "&", binary "|" ],
  [ binary "*", binary "/" ],
  [ binary "+", binary "-" ],
  [ binary "==" ]]

expr :: Parser Expr
expr = buildExpressionParser table term <?> "expression"

term :: Parser Expr
term = str <|> num <|> parens expr

str = T.stringLiteral lexer >>= return . Str
num = (T.float lexer >>= return . Num . JSNum)
   <|> (char '-' >> T.float lexer >>= return . Num . JSNum . negate)






parseJS :: String -> Either ParseError Expr
parseJS str = parse (expr <* eof) "" str

simpleParse str = case parseJS str of
  Right expr -> expr
  Left err   -> error (show err)













prop_showExpr expr = simpleParse (showExpr expr) == expr

