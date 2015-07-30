{-# LANGUAGE OverloadedStrings #-}

module Expr where

import Data.Text (Text)
import qualified Data.Text as T
import Data.Maybe
import Data.Monoid

data Program = Program Strictness [Statement] deriving (Show, Eq)

data SrcLoc = SrcLoc {
  srcFilename :: String,
  srcLine :: Int,
  srcColumn :: Int,
  srcContext :: Maybe Text,
  srcLabel :: [Text]
} deriving (Eq, Ord, Show)

showSrcLoc (SrcLoc file line col cxt _) = "at " ++
  file ++ ":" ++ show line ++ ":" ++ show col ++
  T.unpack (maybe "" (" in " <>) cxt)

type Ident = Text
type Label = Text
type Operator = Text
type ParameterList = [Ident]
type VarDeclaration = (Ident, Maybe Expr)
data Strictness = Strict | NotStrict deriving (Show, Eq)

data ForHeader = For3 (Maybe Expr) (Maybe Expr) (Maybe Expr)
               | For3Var [VarDeclaration] (Maybe Expr) (Maybe Expr)
               | ForIn Expr Expr
               | ForInVar VarDeclaration Expr
  deriving (Show, Eq)

type CaseBlock = ([CaseClause], Maybe DefaultClause, [CaseClause])
data CaseClause = CaseClause Expr [Statement] deriving (Show, Eq)
data DefaultClause = DefaultClause [Statement] deriving (Show, Eq)

data Statement = Block SrcLoc [Statement]
               | LabelledStatement SrcLoc Label Statement
               | VarDecl SrcLoc [VarDeclaration]
               | ExprStmt SrcLoc Expr
               | IfStatement SrcLoc Expr Statement (Maybe Statement)
               | WhileStatement SrcLoc Expr Statement
               | DoWhileStatement SrcLoc Expr Statement
               | For SrcLoc ForHeader Statement
               | ContinueStatement SrcLoc (Maybe Label)
               | BreakStatement SrcLoc (Maybe Label)
               | Return SrcLoc (Maybe Expr)
               | WithStatement SrcLoc Expr Statement
               | SwitchStatement SrcLoc Expr CaseBlock
               | ThrowStatement SrcLoc Expr
               | TryStatement SrcLoc Statement (Maybe Statement) (Maybe Statement)
               | Catch SrcLoc Ident Statement
               | Finally SrcLoc Statement
               | EmptyStatement SrcLoc
               | DebuggerStatement SrcLoc
               | FunDecl SrcLoc Ident ParameterList Strictness FunBody
  deriving (Show, Eq)

type FunBody = [Statement]

type PropertyAssignment = (PropertyName, PropertyValue)
type PropertyName = Text
data PropertyValue = Value Expr
                   | Getter [Statement]
                   | Setter Ident [Statement]
                   deriving (Show, Eq)

data Expr = Num Double
          | INum Integer
          | Str Text
          | Boolean Bool
          | LiteralNull
          | LiteralUndefined
          | This
          | ArrayLiteral [Maybe Expr]
          | ObjectLiteral [PropertyAssignment]
          | RegExp Text Text
          | BinOp Operator Expr Expr
          | UnOp Operator Expr
          | PostOp Operator Expr
          | NewExpr Expr [Expr]
          | ReadVar Ident
          | Assign Expr Text Expr
          | Cond Expr Expr Expr
          | MemberDot Expr Ident  -- e.g., point.x
          | MemberGet Expr Expr   -- e.g., point["x"]
          | FunCall Expr [Expr]
          | FunExpr (Maybe Ident) ParameterList Strictness FunBody
  deriving (Show, Eq)

data Lang = Lang {
  reservedWords :: [Text],
  reservedWordsStrict :: [Text],
  assignOps :: [Text],
  unaryOps :: [Text],
  binaryOps :: [Text],
  postfixOps :: [Text]
}

jsLang :: Lang
jsLang = Lang {
  reservedWords =
    T.words "break do instanceof typeof case else new var catch finally" ++
      T.words "return void continue for switch while debugger" ++
      T.words "function this with default if throw delete in try" ++
      T.words "null true false" ++
      T.words "class enum extends super const export import",
  reservedWordsStrict =
    T.words "implements let private public yield interface package protected static",
  assignOps = [ "=", "+=", "-=", "*=", "/=", "%=",
                "<<=", ">>=", ">>>=", "&=", "^=", "|="],
  unaryOps  = [ "delete", "void", "typeof",
               "+", "-", "~", "!", "++", "--" ],
  binaryOps = [ "+", "-", "*", "/", "%", "==", "!=", "===", "!==",
                "&", "^", "|", "&&", "||",
                "instanceof", "in", ">>", "<<", ">>>",
                ">=", ">", "<=", "<" ],
  postfixOps = [ "++", "--" ]
}

