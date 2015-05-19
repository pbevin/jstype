module Expr where

data Program = Program Strictness [Statement] deriving (Show, Eq)

data SrcLoc = SrcLoc {
  srcFilename :: String,
  srcLine :: Int,
  srcColumn :: Int,
  srcContext :: Maybe String
} deriving (Eq, Ord)

instance Show SrcLoc where
  show (SrcLoc file line col cxt) = "at " ++ file ++ ":" ++ show line ++ ":" ++ show col ++ maybe "" (" in " ++) cxt

type Ident = String
type Label = String
type Operator = String
type ParameterList = [Ident]
type VarDeclaration = (Ident, Maybe Expr)
data Strictness = Strict | NotStrict deriving (Show, Eq)

data ForHeader = For3 (Maybe Expr) (Maybe Expr) (Maybe Expr)
               | For3Var Ident Expr (Maybe Expr) (Maybe Expr)
               | ForIn LHS Expr
               | ForInVar VarDeclaration Expr
  deriving (Show, Eq)

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
               | ThrowStatement SrcLoc Expr
               | TryStatement SrcLoc Statement (Maybe Catch) (Maybe Finally)
               | EmptyStatement SrcLoc
               | DebuggerStatement SrcLoc
  deriving (Show, Eq)

data Catch = Catch SrcLoc Ident Statement deriving (Show, Eq)
data Finally = Finally SrcLoc Statement deriving (Show, Eq)

type FunBody = [Statement]

type LHS = Expr -- XXX
data PropertyName = IdentProp String
                  | StringProp String
                  | NumProp JSNum
                  deriving (Show, Eq)

data Expr = Num JSNum
          | Str String
          | Boolean Bool
          | This
          | ArrayLiteral [Maybe Expr]
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
          | FunDef (Maybe Ident) ParameterList Strictness FunBody
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

newtype JSNum = JSNum Double deriving Show
instance Eq JSNum where
  JSNum a == JSNum b = abs (a-b) < 0.001

jsNaN :: JSNum
jsNaN = JSNum $ 0 / 0


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

instance Real JSNum where
  toRational (JSNum a) = toRational a

instance RealFrac JSNum where
  properFraction (JSNum a) = let (x, y) = properFraction a in (x, JSNum y)

sourceLocation :: Statement -> SrcLoc
sourceLocation stmt = case stmt of
  Block s _ -> s
  LabelledStatement s _ _ -> s
  VarDecl s _ -> s
  ExprStmt s _ -> s
  IfStatement s _ _ _ -> s
  WhileStatement s _ _ -> s
  DoWhileStatement s _ _ -> s
  For s _ _ -> s
  ContinueStatement s _ -> s
  BreakStatement s _ -> s
  Return s _ -> s
  ThrowStatement s _ -> s
  TryStatement s _ _ _ -> s
  EmptyStatement s -> s
  DebuggerStatement s -> s
