module Expr (Program (..),
             Statement (..),
             Expr(..),
             ForHeader(..),
             Catch (..),
             Finally (..),
             PropertyName (..),
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

data ForHeader = For3 (Maybe Expr) (Maybe Expr) (Maybe Expr)
               | For3Var Ident Expr (Maybe Expr) (Maybe Expr)
               | ForIn LHS Expr
               | ForInVar VarDeclaration Expr
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
               | TryStatement Statement (Maybe Catch) (Maybe Finally)
               | EmptyStatement
               | DebuggerStatement
  deriving (Show, Eq)

data Catch = Catch Ident Statement deriving (Show, Eq)
data Finally = Finally Statement deriving (Show, Eq)

type FunBody = [Statement]

type LHS = Expr -- XXX
data PropertyName = IdentProp String
                  | StringProp String
                  | NumProp JSNum
                  deriving (Show, Eq)

data Expr = Num JSNum
          | Str String
          | This
          | ArrayLiteral [Expr]
          | ObjectLiteral [(PropertyName, Expr)]
          | RegularExpression String String
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
                "instanceof", "in", ">>", "<<", ">>>",
                ">=", ">", "<=", "<" ],
  postfixOps = [ "++", "--" ]
}

instance Num JSNum where
  (JSNum a) + (JSNum b) = JSNum (a + b)
  (JSNum a) - (JSNum b) = JSNum (a - b)
  (JSNum a) * (JSNum b) = JSNum (a * b)
  fromInteger n = JSNum $ fromInteger n
  abs (JSNum a) = JSNum $ abs a
  signum (JSNum a) = JSNum $ signum a

instance Fractional JSNum where
  (JSNum a) / (JSNum b) = JSNum (a / b)
  fromRational r = JSNum $ fromRational r

instance Ord JSNum where
  compare (JSNum a) (JSNum b) = compare a b
