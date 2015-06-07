{-# LANGUAGE LambdaCase #-}

module Parse.Statements where

import Text.Parsec hiding (many, (<|>), optional)
import Control.Monad (liftM, when, guard)
import Control.Applicative
import Data.Maybe
import Data.Foldable
import Data.List (sortBy, nub, union, intersect)
import Parse.Types
import Parse.State
import Parse.Lexical
import Expr
import JSNum

prog :: JSParser Program
prog = do
  (strictness, statements) <- statementList
  return $ Program strictness statements

statementList :: JSParser (Strictness, [Statement])
statementList = do
  strictness <- directivePrefix
  withStrictness strictness $ do
    statements <- many statement
    return (strictness, statements)

directivePrefix :: JSParser Strictness
directivePrefix = do
  currentStrictness <- getStrictness
  directives <- many (terminated directive)
  return $ if "use strict" `elem` directives
           then Strict
           else currentStrictness

directive :: JSParser String
directive = try $ do
  str <- quotedString
  guard (str == "use strict")
  return str


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
                     funDecl <?> "function declaration",
                     labelledStmt <?> "label",
                     terminated exprStmt <?> "expression",
                     terminated varDecl <?> "var declaration",
                     ifStmt <?> "if",
                     forStmt <?> "for",
                     whileStmt <?> "while",
                     withStmt <?> "with",
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
  stmts <- braces (many statement)
  return $ case stmts of
    [single] -> single
    _ -> Block loc stmts

realblock :: JSParser Statement
realblock = Block <$> srcLoc <*> braces (many statement)

funDecl :: JSParser Statement
funDecl = do
  loc <- srcLoc
  try $ keyword "function"
  name <- identifier <?> "function name"
  ifStrict $ guard (legalIdentifier name)
  params <- paramList
  (strictness, stmts) <- withFunctionContext (Just name) (withoutInsideIteration $ braces statementList) <?> "function body"
  return $ FunDecl loc name params strictness stmts

paramList :: JSParser [Ident]
paramList = do
  params <- parens (identifier `sepBy` comma) <?> "parameter list"
  ifStrict $ guard (all legalIdentifier params && allDistinct params)
  return params
    where allDistinct xs = (xs == nub xs)

labelledStmt :: JSParser Statement
labelledStmt = try $ do
  loc <- srcLoc
  lab <- identifier
  tok ":"
  LabelledStatement loc lab <$> withLabel lab statement

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
returnStmt = ifInsideFunction $ do
  pos1 <- getPosition
  reserved "return"
  pos2 <- getPosition

  Return <$> srcLoc <*> if sameLine pos1 pos2
                        then optionMaybe expr
                        else pure Nothing

withStmt :: JSParser Statement
withStmt = keyword "with" >>
             WithStatement <$> srcLoc <*> parens expr <*> statement

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
for3var = keyword "var" >> do
  assigns <- varAssign `sepBy1` comma
  For3Var assigns
          <$> (tok ";" >> optionMaybe expr)
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
throwStmt = do
  pos1 <- getPosition
  keyword "throw"
  pos2 <- getPosition
  if sameLine pos1 pos2
  then ThrowStatement <$> srcLoc <*> expr
  else unexpected "newline"

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
memberExpr p = newExpr p <|> baseMemberExpr p

newExpr :: JSParser Expr -> JSParser Expr
newExpr p = do
  keyword "new"
  NewExpr <$> memberExpr p <*> args
    where args = fromMaybe [] <$> optional (parens argumentList)

baseMemberExpr :: JSParser Expr -> JSParser Expr
baseMemberExpr p = do
  base <- functionExpr <|> p
  extras <- many (dotExt <|> arrayExt)
  return $ foldl (flip ($)) base extras

dotExt :: JSParser (Expr -> Expr)
dotExt = try $ do
  lexeme (char '.')
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
  ifStrict $ forM_ name $ \n ->
    guard (legalIdentifier n)
  params <- paramList
  (strictness, stmts) <- withFunctionContext name (withoutInsideIteration $ braces statementList) <?> "function body"
  return $ FunExpr name params strictness stmts

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
            ifStrict (guard $ legalModification op e)
            return $ PostOp op e

unaryExpr :: JSParser Expr -> JSParser Expr
unaryExpr p = (try unop <|> p) <?> "unary expr"
  where unop = do
          op <- choice $ map (try . string) $ sortBy reverseLength $ unaryOps jsLang
          whiteSpace
          e <- unaryExpr p
          ifStrict (guard $ legalModification op e)
          return $ UnOp op e

legalModification :: String -> Expr -> Bool
legalModification "++" (ReadVar x) = legalIdentifier x
legalModification "--" (ReadVar x) = legalIdentifier x
legalModification _ _ = True

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
assignExpr p = p >>= rest
  where rest :: Expr -> JSParser Expr
        rest e = do op <- do tok "=" <|> assignOp
                    ifStrict $ do
                      guard (e /= ReadVar "eval")
                      guard (e /= ReadVar "arguments")
                    Assign e op <$> assignExpr p
             <|> return e

assignOp :: JSParser String
assignOp = choice $ map op $ assignOps jsLang
  where op name = try (tok name) >> return name

simple :: JSParser Expr
simple = parens expr
     <|> arrayLiteral
     <|> objectLiteral
     <|> regexLiteral
     <|> this
     <|> literal
     <|> num
     <|> var
     <|> Str <$> quotedString

this :: JSParser Expr
this = try $ keyword "this" >> return This

var :: JSParser Expr
var = ReadVar <$> identifier

arrayLiteral :: JSParser Expr
arrayLiteral = ArrayLiteral <$> brackets arrayContents

arrayContents :: JSParser [Maybe Expr]
arrayContents = do
  (++) <$> elision <*> (content <|> return [])
  where elision = many (tok "," >> return Nothing)
        content = do val <- assignmentExpr
                     (tok "," >> more val) <|> return [Just val]
                           where more v = (Just v :) <$> arrayContents


objectLiteral :: JSParser Expr
objectLiteral =
  let propertyAssignment = getter <|> setter <|> nameValuePair
  in do
    assignments <- braces (propertyAssignment `sepBy` comma)
    guard (noDuplicateAccessorKeys assignments)
    ifStrict $ guard (noDuplicateKeys assignments)
    return $ ObjectLiteral assignments

noDuplicateAccessorKeys :: [PropertyAssignment] -> Bool
noDuplicateAccessorKeys assignments =
  noMultipleGetters && noMultipleSetters && noAccessorWithValue
    where
      noMultipleGetters = getters == nub getters
      noMultipleSetters = setters == nub setters
      noAccessorWithValue = null $ values `intersect` (getters `union` setters)
      getters, setters, values :: [String]
      getters = map fst $ filter (isGetter . snd) assignments
      setters = map fst $ filter (isSetter . snd) assignments
      values  = map fst $ filter (isValue  . snd) assignments
      isGetter (Getter _) = True
      isGetter _ = False
      isSetter (Setter _ _) = True
      isSetter _ = False
      isValue (Value _) = True
      isValue _ = False

noDuplicateKeys :: [PropertyAssignment] -> Bool
noDuplicateKeys assignments =
  let keys = map fst assignments
  in keys == nub keys

propertyName :: JSParser PropertyName
propertyName = identifier
           <|> quotedString
           <|> numberText

nameValuePair :: JSParser PropertyAssignment
nameValuePair = do
  name <- propertyName
  tok ":"
  val <- assignmentExpr
  return (name, Value val)

getter :: JSParser PropertyAssignment
getter = try $ do
  tok "get"
  name <- propertyName
  tok "("
  tok ")"
  let cxt = Just ("get " ++ name)
  stmts <- braces (withFunctionContext cxt $ many statement)
  return (name, Getter stmts)

setter :: JSParser PropertyAssignment
setter = try $ do
  tok "set"
  name <- propertyName
  tok "("
  param <- identifier
  ifStrict $ guard (legalIdentifier param)
  tok ")"
  let cxt = Just ("set " ++ name)
  stmts <- braces (withFunctionContext cxt $ many statement)
  return (name, Setter param stmts)

legalIdentifier :: Ident -> Bool
legalIdentifier param = (param /= "eval" && param /= "arguments")

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
escape = oneOf "'\"\\\n"
     <|> liftM singleCharEscape (oneOf "bfnrtv")
     <|> (char 'u' >> unicodeEscape)

singleCharEscape :: Char -> Char
singleCharEscape 'b' = '\b'
singleCharEscape 't' = '\t'
singleCharEscape 'n' = '\n'
singleCharEscape 'v' = '\v'
singleCharEscape 'f' = '\f'
singleCharEscape 'r' = '\r'
singleCharEscape ch  = ch

singleQuotedString :: JSParser String
singleQuotedString = do
  char '\''
  str <- many (noneOf "'\\" <|> (char '\\' >> escape))
  char '\''
  whiteSpace
  return str

num :: JSParser Expr
num = Num <$> numericLiteral

literal :: JSParser Expr
literal = try $ (keyword "true"  >> return (Boolean True))
            <|> (keyword "false" >> return (Boolean False))
            <|> (keyword "null"  >> return LiteralNull)

numericLiteral :: JSParser JSNum
numericLiteral = liftM JSNum number
