module Parse.Statements where

import Text.Parsec hiding (many, (<|>), optional)
import Control.Monad (replicateM, liftM, when)
import Control.Applicative
import Data.Char
import Data.List (sortBy)
import Numeric
import Parse.Types
import Parse.State
import Parse.Lexical
import Expr

prog :: JSParser Program
prog = Program <$> statementList

statementList :: JSParser [Statement]
statementList = many statement

terminated :: JSParser a -> JSParser a
terminated p = do
  pos1 <- getPosition
  result <- p
  pos2 <- getPosition

  when (sameLine pos1 pos2) $
    semicolon <|> lookAhead (eof <|> skip "}")

  return result

statement :: JSParser Statement
statement = choice [ block <?> "block",
                     labelledStmt <?> "label",
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

labelledStmt :: JSParser Statement
labelledStmt = try $ do
  loc <- srcLoc
  label <- identifier
  tok ":"
  LabelledStatement loc label <$> withLabel label statement

varDecl :: JSParser Statement
varDecl = try (keyword "var" >> VarDecl <$> srcLoc <*> varAssign `sepBy1` comma) <?> "variable declaration"

varAssign, varAssignNoIn :: JSParser (String, Maybe Expr)
varAssign = do
  name <- identifier
  assign name <|> return (name, Nothing)
    where assign x = do
            tok "="
            e <- assignmentExpr
            return (x, Just e)

varAssignNoIn = withoutInKeyword varAssign

returnStmt :: JSParser Statement
returnStmt = do
  pos1 <- getPosition
  reserved "return"
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
  WhileStatement <$> srcLoc
                 <*> parens expr
                 <*> withInsideIteration statement

doWhileStmt :: JSParser Statement
doWhileStmt = try $ keyword "do" >> do
  loc <- srcLoc
  stmt <- withInsideIteration statement
  keyword "while"
  e <- parens expr
  return $ DoWhileStatement loc e stmt


forStmt :: JSParser Statement
forStmt = try (keyword "for") >>
  For <$> srcLoc
      <*> forHeader
      <*> withInsideIteration statement

forHeader :: JSParser ForHeader
forHeader = parens (forinvar <|> try forin <|> for3 <|> for3var)

forinvar, forin, for3, for3var :: JSParser ForHeader
forinvar = try $ keyword "var" >> ForInVar <$> varAssignNoIn <*> (keyword "in" >> expr)
forin = ForIn <$> exprNoIn <*> (keyword "in" >> expr)
for3 = For3 <$> optionMaybe exprNoIn
            <*> (tok ";" >> optionMaybe expr)
            <*> (tok ";" >> optionMaybe expr)
for3var = keyword "var" >>
  For3Var <$> identifier <* tok "="
          <*> exprNoIn
          <*> (tok ";" >> optionMaybe expr)
          <*> (tok ";" >> optionMaybe expr)

exprStmt :: JSParser Statement
exprStmt = ExprStmt <$> srcLoc <*> expr

emptyStmt :: JSParser Statement
emptyStmt = semicolon >> EmptyStatement <$> srcLoc

breakStmt :: JSParser Statement
breakStmt = ifInsideIteration $ keyword "break" >>
  BreakStatement <$> srcLoc <*> optional (fromLabelSet identifier)

continueStmt :: JSParser Statement
continueStmt = ifInsideIteration $ do
  pos1 <- getPosition
  keyword "continue"
  pos2 <- getPosition
  if sameLine pos1 pos2
  then ContinueStatement <$> srcLoc
                         <*> optional (fromLabelSet identifier)
  else ContinueStatement <$> srcLoc <*> pure Nothing


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
  return $ SrcLoc (sourceName pos) (sourceLine pos) (sourceColumn pos) cxt



exprNoIn :: JSParser Expr
exprNoIn = withoutInKeyword expr

expr :: JSParser Expr
expr = assignmentExpr `chainl1` commaExpr
  where commaExpr = tok "," >> return (BinOp ",")

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
  stmts <- withFunctionContext name (withoutInsideIteration $ braces statementList) <?> "function body"
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
            op <- choice $ map (try . tok) $ postfixOps jsLang
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
            tok "?"
            ifTrue <- expr
            tok ":"
            ifFalse <- expr
            return $ Cond test ifTrue ifFalse

assignExpr :: JSParser Expr -> JSParser Expr
assignExpr p = do
  e <- p
  assignment e <|> return e

assignment :: Expr -> JSParser Expr
assignment lhs = do
  op <- tok "=" <|> assignOp
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
     <|> bool
     <|> num
     <|> var
     <|> Str <$> quotedString

this :: JSParser Expr
this = try $ keyword "this" >> return This

var :: JSParser Expr
var = liftM ReadVar identifier

arrayLiteral :: JSParser Expr
arrayLiteral = ArrayLiteral <$> brackets arrayContents

arrayContents :: JSParser [Maybe Expr]
arrayContents = nonEmptyContents <|> elisionFirst <|> return []
  where nonEmptyContents = do
          first <- assignmentExpr
          rest <- many (tok "," >> optional assignmentExpr)
          return $ (Just first) : rest
        elisionFirst = do
          rest <- many1 (tok "," >> optional assignmentExpr)
          return $ Nothing : rest


objectLiteral :: JSParser Expr
objectLiteral = ObjectLiteral <$> braces (propertyAssignment `sepBy` comma)
  where
    propertyAssignment = do
      name <- IdentProp <$> identifier
                <|> StringProp <$> quotedString
                <|> NumProp <$> numericLiteral
      tok ":"
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


-- ref 7.8.4
quotedString :: JSParser String
quotedString = doubleQuotedString <|> singleQuotedString

doubleQuotedString :: JSParser String
doubleQuotedString = do
  char '"'
  str <- many (noneOf "\"\\" <|> (char '\\' >> escape))
  char '"'
  whiteSpace
  return str

escape :: JSParser Char
escape = oneOf "'\"\\"
     <|> liftM singleCharEscape (oneOf "bfnrtv")
     <|> (char 'u' >> unicodeEscape)

singleCharEscape :: Char -> Char
singleCharEscape 'b' = '\b'
singleCharEscape 't' = '\t'
singleCharEscape 'n' = '\n'
singleCharEscape 'v' = '\v'
singleCharEscape 'f' = '\f'
singleCharEscape 'r' = '\r'

unicodeEscape :: JSParser Char
unicodeEscape = liftM (chr . fst . head . readHex) $ replicateM 4 hexDigit

singleQuotedString :: JSParser String
singleQuotedString = do
  char '\''
  str <- many (noneOf "'\\" <|> (char '\\' >> anyChar))
  char '\''
  whiteSpace
  return str

num :: JSParser Expr
num = Num <$> numericLiteral

bool :: JSParser Expr
bool = try $ (keyword "true" >> return (Boolean True))
               <|> (keyword "false" >> return (Boolean False))

numericLiteral :: JSParser JSNum
numericLiteral = liftM (JSNum . read) number

assignOp :: JSParser String
assignOp = choice $ map op $ assignOps jsLang
  where op name = try (tok name) >> return name
