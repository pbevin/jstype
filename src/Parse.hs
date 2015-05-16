module Parse (parseJS, parseJS', simpleParse, parseExpr, prop_showExpr, prop_showProg, disprove, disprove') where

import Control.Monad
import Control.Applicative hiding (many, optional, (<|>))
import Test.QuickCheck
import Text.Parsec hiding (newline)
import Text.Parsec.String
import qualified Text.Parsec.Token as T
import Text.Parsec.Language (javaStyle)
import Data.Maybe
import Data.List
import Data.Char
import ShowExpr
import Expr

import Debug.Trace

type ParseState = (Bool, Maybe String) -- "in" allowed, context
type JSParser = Parsec String ParseState

jsParse :: JSParser a -> SourceName -> String -> Either ParseError a
jsParse p = runP p (True, Nothing)

parseJS :: String -> Either ParseError Program
parseJS str = parseJS' str ""

parseJS' :: String -> String -> Either ParseError Program
parseJS' str filename = jsParse (whiteSpace >> prog <* eof) filename str

simpleParse :: String -> Program
simpleParse str = case parseJS str of
  Right p  -> p
  Left err -> error (show err)

parseExpr :: String -> Expr
parseExpr str = case jsParse (expr <* eof) "" str of
  Right e  -> e
  Left err -> error (show err)


sameLine :: SourcePos -> SourcePos -> Bool
sameLine pos1 pos2 = sourceLine pos1 == sourceLine pos2

identStart, identLetter :: [Char]
identStart  = ['a'..'z'] ++ ['A'..'Z'] ++ "$_"
identLetter = identStart ++ ['0'..'9']


javascript :: T.LanguageDef st
javascript = javaStyle
              { T.reservedNames = reservedWords jsLang
              , T.identStart = oneOf identStart
              , T.identLetter = oneOf identLetter
              , T.caseSensitive = True }


lexer :: T.TokenParser st
lexer = T.makeTokenParser javascript

parens, braces, brackets :: JSParser a -> JSParser a
parens = T.parens lexer
braces = T.braces lexer
brackets = T.brackets lexer


identifier :: JSParser String
identifier = T.identifier lexer
integer :: JSParser Integer
integer = T.integer lexer
float :: JSParser Double
float = T.float lexer
reserved :: String -> JSParser ()
reserved = T.reserved lexer
whiteSpace :: JSParser ()
whiteSpace = T.whiteSpace lexer

spaceNotNewline :: Char -> Bool
spaceNotNewline ch = isSpace ch && ch /= '\n'

noNewline :: JSParser ()
noNewline = void (satisfy spaceNotNewline)

allOps :: [String]
allOps = sortBy reverseLength $ nub allJsOps
  where allJsOps = assignOps jsLang ++ unaryOps jsLang ++ binaryOps jsLang ++ postfixOps jsLang

reverseLength :: String -> String -> Ordering
reverseLength a b = compare (length b) (length a)

lexeme :: String -> JSParser String
lexeme str = string str <* whiteSpace

keyword :: String -> JSParser ()
keyword str = reserved str >> whiteSpace

resOp :: JSParser String
resOp = do
  op <- choice (map (try . string) allOps)
  whiteSpace
  return op

skip :: String -> JSParser ()
skip = void . lexeme

comma, semicolon :: JSParser ()
comma = skip ","
semicolon = skip ";"


disableInKeyword, enableInKeyword :: JSParser ()
disableInKeyword = modifyState $ \(_, cxt) -> (False, cxt)
enableInKeyword  = modifyState $ \(_, cxt) -> (True, cxt)

withoutInKeyword :: JSParser a -> JSParser a
withoutInKeyword p = disableInKeyword *> p <* enableInKeyword

removeIn :: [String] -> JSParser [String]
removeIn ops = do
  (inKeywordEnabled, _) <- getState
  return $ if inKeywordEnabled
           then ops
           else ops \\ ["in"]

withFunctionContext :: Maybe String -> JSParser a -> JSParser a
withFunctionContext fname p =
  let here = fromMaybe "anonymous function" fname
  in do
    (a, cxt) <- getState
    putState (a, Just here)
    result <- p
    putState (a, cxt)
    return result

currentContext :: JSParser (Maybe String)
currentContext = snd <$> getState

prog :: JSParser Program
prog = Program <$> statementList

statementList :: JSParser [Statement]
statementList = many statement

terminated :: JSParser a -> JSParser a
terminated p = do
  pos1 <- getPosition
  result <- p
  pos2 <- getPosition

  when (sameLine pos1 pos2) $ do
    semicolon <|> lookAhead (eof <|> skip "}")

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
  loc <- srcLoc
  stmts <- braces statementList
  return $ case stmts of
    [single] -> single
    _ -> Block loc stmts

realblock :: JSParser Statement
realblock = Block <$> srcLoc <*> braces statementList


varDecl :: JSParser Statement
varDecl = try (keyword "var" >> VarDecl <$> srcLoc <*> varAssign `sepBy1` comma) <?> "variable declaration"

varAssign, varAssignNoIn :: JSParser (String, Maybe Expr)
varAssign = do
  name <- identifier
  assign name <|> return (name, Nothing)
    where assign x = do
            lexeme "="
            e <- assignmentExpr
            return (x, Just e)

varAssignNoIn = withoutInKeyword varAssign

returnStmt :: JSParser Statement
returnStmt = do
  pos1 <- getPosition
  try (keyword "return")
  pos2 <- getPosition

  Return <$> srcLoc <*> if sameLine pos1 pos2
                        then optionMaybe expr
                        else pure Nothing

ifStmt :: JSParser Statement
ifStmt = do
  try $ keyword "if"
  loc <- srcLoc
  test <- parens expr
  ifTrue <- statement
  ifFalse <- try elseClause <|> return Nothing
  return $ IfStatement loc test ifTrue ifFalse

elseClause :: JSParser (Maybe Statement)
elseClause = try (keyword "else" >> Just <$> statement)

whileStmt :: JSParser Statement
whileStmt = try $ keyword "while" >>
  WhileStatement <$> srcLoc <*> parens expr <*> statement

doWhileStmt :: JSParser Statement
doWhileStmt = try $ keyword "do" >> do
  loc <- srcLoc
  stmt <- statement
  keyword "while"
  e <- parens expr
  return $ DoWhileStatement loc e stmt


forStmt :: JSParser Statement
forStmt = try (keyword "for") >> For <$> srcLoc <*> forHeader <*> statement

forHeader :: JSParser ForHeader
forHeader = parens (forinvar <|> try forin <|> for3)

forinvar, forin, for3 :: JSParser ForHeader
forinvar = keyword "var" >> ForInVar <$> varAssignNoIn <*> (keyword "in" >> expr)
forin = ForIn <$> exprNoIn <*> (keyword "in" >> expr)
for3 = For3 <$> optionMaybe exprNoIn <*>
                (lexeme ";" >> optionMaybe expr) <*>
                (lexeme ";" >> optionMaybe expr)

exprStmt :: JSParser Statement
exprStmt = ExprStmt <$> srcLoc <*> expr

emptyStmt :: JSParser Statement
emptyStmt = semicolon >> EmptyStatement <$> srcLoc

breakStmt :: JSParser Statement
breakStmt = keyword "break" >> BreakStatement <$> srcLoc

continueStmt :: JSParser Statement
continueStmt = keyword "continue" >> ContinueStatement <$> srcLoc

throwStmt :: JSParser Statement
throwStmt = try (keyword "throw") >> ThrowStatement <$> srcLoc <*> expr

tryStmt :: JSParser Statement
tryStmt = try (keyword "try") >> TryStatement <$> srcLoc <*> realblock
             <*> optionMaybe catch <*> optionMaybe finally
    where catch = try (keyword "catch") >>
                     Catch <$> srcLoc <*> parens identifier <*> realblock
          finally = try (keyword "finally") >>
                     Finally <$> srcLoc <*> realblock




debuggerStmt :: JSParser Statement
debuggerStmt = keyword "debugger" >> DebuggerStatement <$> srcLoc






srcLoc :: JSParser SrcLoc
srcLoc = do
  pos <- getPosition
  cxt <- currentContext
  return $ SrcLoc (sourceName pos) (sourceLine pos) (sourceColumn pos) $ cxt



exprNoIn :: JSParser Expr
exprNoIn = withoutInKeyword expr

expr :: JSParser Expr
expr = assignmentExpr `chainl1` commaExpr
  where commaExpr = lexeme "," >> return (BinOp ",")

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

memberExpr :: JSParser Expr -> JSParser Expr
memberExpr p = (try (keyword "new") >> NewExpr <$> memberExpr p <*> parens argumentList)
           <|> baseMemberExpr p

baseMemberExpr :: JSParser Expr -> JSParser Expr
baseMemberExpr p = do
  base <- functionExpr <|> p
  extras <- many (dotExt <|> arrayExt)
  return $ foldl (flip ($)) base extras

dotExt :: JSParser (Expr -> Expr)
dotExt = try $ do
  char '.'
  name <- identifier
  return (`MemberDot` name)

arrayExt :: JSParser (Expr -> Expr)
arrayExt = try $ do
  x <- brackets expr
  return (`MemberGet` x)


functionExpr :: JSParser Expr
functionExpr = do
  try $ keyword "function"
  name <- optionMaybe identifier <?> "function name"
  params <- parens (identifier `sepBy` comma) <?> "parameter list"
  stmts <- withFunctionContext name (braces statementList) <?> "function body"
  return $ FunDef name params stmts

callExpr :: JSParser Expr -> JSParser Expr
callExpr p = do
  base <- p
  addons base
    where
      addons :: Expr -> JSParser Expr
      addons base = (parens argumentList >>= \args -> addons $ FunCall base args)
                <|> ((char '.' >> identifier) >>= \name -> addons $ MemberDot base name)
                <|> (brackets expr >>= \e -> addons $ MemberGet base e)
                <|> return base

argumentList :: JSParser [Expr]
argumentList = assignmentExpr `sepBy` comma

postfixExpr :: JSParser Expr -> JSParser Expr
postfixExpr p = do
  pos1 <- getPosition
  e <- p
  pos2 <- getPosition

  if sameLine pos1 pos2
  then try (postfix e) <|> return e
  else return e
    where postfix e = do
            op <- choice $ map (try . lexeme) $ postfixOps jsLang
            whiteSpace
            return $ PostOp op e

unaryExpr :: JSParser Expr -> JSParser Expr
unaryExpr p = (try unop <|> p) <?> "unary expr"
  where unop = do
          op <- choice $ map (try . string) $ sortBy reverseLength $ unaryOps jsLang
          whiteSpace
          e <- unaryExpr p
          return $ UnOp op e

binOps :: [String] -> JSParser Expr -> JSParser Expr
binOps allowedOps p = do
  ops <- removeIn allowedOps
  chainl1 p $ try $ do
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
  e <- p
  assignment e <|> return e

assignment :: Expr -> JSParser Expr
assignment lhs = do
  op <- lexeme "=" <|> assignOp
  rhs <- expr
  return $ Assign lhs op rhs

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
     <|> Str <$> quotedString

this :: JSParser Expr
this = try $ keyword "this" >> return This

var :: JSParser Expr
var = liftM ReadVar identifier

arrayLiteral :: JSParser Expr
arrayLiteral = ArrayLiteral <$> brackets (expr `sepBy` comma)

objectLiteral :: JSParser Expr
objectLiteral = ObjectLiteral <$> braces (propertyAssignment `sepBy` comma)
  where
    propertyAssignment = do
      name <- IdentProp <$> identifier
                <|> StringProp <$> quotedString
                <|> NumProp <$> numericLiteral
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

regexFirstChar, regexChar, regexBackslash, regexClass :: JSParser String
regexFirstChar = tostr (noneOf "*\\/[\n")
             <|> regexBackslash
             <|> regexClass

regexChar      = tostr (noneOf "\\/\n")
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












prop_showProg :: Program -> Property
prop_showProg p = counterexample (ppcode p) $
  simpleParse (ppcode p) == p

prop_showExpr :: Expr -> Property
prop_showExpr e = counterexample (ppcode e) $
  parseExpr (ppcode e) == e

disprove :: Program -> IO ()
disprove p = do
  print p
  putStrLn $ ppcode p
  print (simpleParse $ ppcode p)

disprove' :: Expr -> IO ()
disprove' e = do
  print e
  putStrLn $ ppcode e
  print (parseExpr $ ppcode e)
