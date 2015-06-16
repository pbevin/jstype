module Core where

import Data.Maybe
import Expr

type CoreStatement = CoreStmt SrcLoc
data CoreStmt a  = CoreBinding [(Ident, Expr)] [CoreStmt a]
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
desugar stmts = CoreBinding (declBindings stmts) $ map convert stmts

convert :: Statement -> CoreStatement
convert stmt = Unconverted stmt


declBindings :: [Statement] -> [(Ident, Expr)]
declBindings stmts = [ (name, LiteralUndefined) | name <- concatMap searchVariables stmts ]
                  ++ concatMap searchFunctions stmts

















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

