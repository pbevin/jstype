module Expr (Program (..),
             Statement (..),
             Expr(..),
             JSNum(..),
             Lang(..),
             Ident,
             jsLang) where

newtype JSNum = JSNum Double deriving Show
instance Eq JSNum where
  JSNum a == JSNum b = abs (a-b) < 0.001

newtype Program = Program [Statement] deriving (Show, Eq)

type Ident = String
type Operator = String
type ParameterList = [Ident]
type VarDeclaration = (Ident, Maybe Expr)

data ForHeader = For3 Expr Expr Expr
               | For3Var Ident Expr Expr Expr
               | ForIn LHS Expr
               | ForInVar Ident LHS Expr
  deriving (Show, Eq)

data Statement = Block [Statement]
               | VarDecl [VarDeclaration]
               | ExprStmt Expr
               | IfStatement Expr Statement (Maybe Statement)
               | WhileStatement Expr Statement
               | DoWhileStatement Expr Statement
               | For ForHeader Statement
               -- | ForStatement ... (4 cases)
               | ContinueStatement
               | BreakStatement
               | Return (Maybe Expr)
               -- | WithStatement
               | IdentifierStatement Ident Statement
               | SwitchStatement
               | ThrowStatement Expr
               | TryStatement
               | EmptyStatement
               | DebuggerStatement
  deriving (Show, Eq)

type FunBody = [Statement]

type LHS = Expr -- XXX

data Expr = Num JSNum
          | Str String
          | This
          | ArrayLiteral [Expr]
          | BinOp Operator Expr Expr
          | UnOp Operator Expr
          | PostOp Operator Expr
          | NewExpr Expr [Expr]
          | ReadVar Ident
          | Assign Expr String Expr
          | Cond Expr Expr Expr
          | MemberDot Expr Ident  -- e.g., point.x
          | MemberGet Expr Expr   -- e.g., point["x"]
          | FunCall Expr [Expr]
          | FunDef (Maybe Ident) ParameterList FunBody
  deriving (Show, Eq)

data Lang = Lang {
  reservedWords :: [String],
  assignOps :: [String],
  unaryOps :: [String],
  binaryOps :: [String],
  postfixOps :: [String]
}

jsLang :: Lang
jsLang = Lang {
  reservedWords  =
    words "break do instanceof typeof case else new var catch finally" ++
      words "return void continue for switch while debugger" ++
      words "function this with default if throw delete in try" ++
      words "class enum extends super const export import" ++
      words "implements let private public yield interface package protected static",
  assignOps = [ "=", "+=", "-=", "*=", "/=", "%=",
                "<<=", ">>=", ">>>=", "&=", "^=", "|="],
  unaryOps  = [ "delete", "void", "typeof",
               "+", "-", "~", "!", "++", "--" ],
  binaryOps = [ "+", "-", "*", "/", "%", "==", "!=", "===", "!==",
                "&", "^", "|", "&&", "||",
                "in", "instanceof", ">>", "<<", ">>>",
                ">=", ">", "<=", "<" ],
  postfixOps = [ "++", "--" ]
}
