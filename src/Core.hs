module Core where

import Data.Maybe
import Expr

data DBIType = DBIGlobal | DBIFunction | DBIEval deriving (Show, Eq)
type CoreStatement = CoreStmt SrcLoc
data CoreStmt a  = CoreBind  DBIType [(Ident, Expr)] (CoreStmt a)
                 | CoreBlock [CoreStmt a]
                 | CoreExpr  a Expr
                 | CoreLoop  a Expr Expr (CoreStmt a)
                 | CoreBreak a (Maybe Label)
                 | CoreCont  a (Maybe Label)
                 | CoreRet   a Expr
                 | CoreCase  a CaseBlock
                 | CoreLabel a Label (CoreStmt a)
                 | CoreTry   a (CoreStmt a) (Maybe (Ident, CoreStmt a)) (Maybe (CoreStmt a))
                 | Unconverted Statement
                 deriving (Show, Eq)


desugar :: [Statement] -> CoreStatement
desugar stmts = CoreBind DBIGlobal (declBindings stmts) . coreBlock $ map convert stmts
  where coreBlock [s] = s
        coreBlock xs  = CoreBlock xs

convert :: Statement -> CoreStatement
convert stmt = case stmt of
  WhileStatement loc cond body   -> convertWhile loc cond body
  DoWhileStatement loc cond body -> convertDoWhile loc cond body
  otherwise                      -> Unconverted stmt


convertWhile :: SrcLoc -> Expr -> Statement -> CoreStatement
convertWhile loc expr stmt = CoreLoop loc expr LiteralUndefined (convert stmt)

convertDoWhile :: SrcLoc -> Expr -> Statement -> CoreStatement
convertDoWhile loc expr stmt = CoreBlock [ convert stmt, convertWhile loc expr stmt ]


declBindings :: [Statement] -> [(Ident, Expr)]
declBindings stmts = [ (name, LiteralUndefined) | name <- concatMap searchVariables stmts ]
                  ++ concatMap searchFunctions stmts


sourceLocation :: CoreStatement -> SrcLoc
sourceLocation stmt = case stmt of
  CoreBind _ _ _   -> error "no srcloc for corebind"
  CoreBlock _      -> error "no srcloc for coreblock"
  CoreExpr a _     -> a
  Unconverted s    -> sourceLocationUnconverted s

sourceLocationUnconverted :: Statement -> SrcLoc
sourceLocationUnconverted stmt = case stmt of
  Block loc _               -> loc
  LabelledStatement loc _ _ -> loc
  VarDecl loc _             -> loc
  ExprStmt loc _            -> loc
  IfStatement loc _ _ _     -> loc
  WhileStatement loc _ _    -> loc
  DoWhileStatement loc _ _  -> loc
  For loc _ _               -> loc
  ContinueStatement loc _   -> loc
  BreakStatement loc _      -> loc
  Return loc _              -> loc
  WithStatement loc _ _     -> loc
  ThrowStatement loc _      -> loc
  TryStatement loc _ _ _    -> loc
  EmptyStatement loc        -> loc
  DebuggerStatement loc     -> loc















searchFunctions :: Statement -> [(Ident, Expr)]
searchFunctions = walkStatement fnFinder (const [])

fnFinder :: Statement -> [(Ident, Expr)]
fnFinder stmt = case stmt of
  FunDecl _ i p st body -> [ (i, FunExpr (Just i) p st body) ]
  _                     -> []

searchVariables :: Statement -> [Ident]
searchVariables = walkStatement varFinder (const [])

varFinder :: Statement -> [Ident]
varFinder (VarDecl _ ds) = map fst ds
varFinder _ = []

walkStatement :: (Statement -> [a]) -> (Expr -> [a]) -> Statement -> [a]
walkStatement sv ev = walk where
  walk stmt = sv stmt ++ case stmt of
    Block _ ss               -> concatMap walk ss
    VarDecl _ vds -> concatMap ewalk (mapMaybe snd vds)
    ExprStmt _ e -> ewalk e
    LabelledStatement _ _ s  -> walk s
    IfStatement _ e s1 Nothing -> ewalk e ++ walk s1
    IfStatement _ e s1 (Just s2) -> ewalk e ++ walk s1 ++ walk s2
    WhileStatement _ e s     -> ewalk e ++ walk s
    DoWhileStatement _ e s   -> ewalk e ++ walk s
    Return _ (Just e)        -> ewalk e
    WithStatement _ e s      -> ewalk e ++ walk s
    For l h s                -> hwalk l h ++ walk s
    TryStatement _ s c f     -> walk s ++ concatMap walk (catMaybes [c, f])
    Catch _ _ s              -> walk s
    Finally _ s              -> walk s

    _                        -> []
  ewalk = walkExpr sv ev  -- not needed?
  hwalk l header = case header of
    For3 e1 e2 e3            -> concatMap ewalk (catMaybes [e1, e2, e3])
    For3Var decls e2 e3      -> walk (VarDecl l decls) ++ concatMap ewalk (catMaybes [e2, e3])
    ForIn e1 e2              -> ewalk e1 ++ ewalk e2
    ForInVar decl e          -> walk (VarDecl l [decl]) ++ ewalk e

walkExpr :: (Statement -> [a]) -> (Expr -> [a]) -> Expr -> [a]
walkExpr _ ev = walk where
  walk expr = ev expr ++ case expr of
    ArrayLiteral es  -> concatMap walk (catMaybes es)
    ObjectLiteral as -> concatMap walkPropAss as
    BinOp _ e1 e2    -> walk e1 ++ walk e2
    UnOp _ e         -> walk e
    PostOp _ e       -> walk e
    NewExpr e es     -> walk e ++ concatMap walk es
    Assign e1 _ e2   -> walk e1 ++ walk e2
    Cond e1 e2 e3    -> walk e1 ++ walk e2 ++ walk e3
    MemberDot e _    -> walk e
    MemberGet e1 e2  -> walk e1 ++ walk e2
    FunCall e es     -> walk e ++ concatMap walk es
    _                -> []

  walkPropAss (_, Value e) = walk e
  walkPropAss (_, _) = []

