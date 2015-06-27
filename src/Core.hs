module Core where

import Control.Arrow
import Data.Maybe
import Expr

data DBIType = DBINone | DBIGlobal | DBIFunction | DBIEval deriving (Show, Eq)
data EnvType = DeclarativeEnv | ObjectEnv Expr deriving (Show, Eq)
type CoreStatement = CoreStmt SrcLoc
data CoreStmt a  = CoreBind  DBIType EnvType [(Ident, Expr)] (CoreStmt a)
                 | CoreBlock [CoreStmt a]
                 | CoreExpr  a Expr
                 | CoreLoop  a Expr Expr (CoreStmt a)
                 | CoreForIn a Expr Expr (CoreStmt a)
                 | CoreBreak a (Maybe Label)
                 | CoreCont  a (Maybe Label)
                 | CoreRet   a Expr
                 | CoreCase  a Expr [(Maybe Expr, CoreStmt a)]
                 | CoreIf    a Expr (CoreStmt a) (Maybe (CoreStmt a))
                 | CoreLabel a Label (CoreStmt a)
                 | CoreTry   a (CoreStmt a) (Maybe (Ident, CoreStmt a)) (Maybe (CoreStmt a))
                 | CoreThrow a Expr
                 deriving (Show, Eq)

desugar :: [Statement] -> CoreStatement
desugar stmts = CoreBind DBIGlobal DeclarativeEnv (declBindings stmts) . coreBlock $ map convert stmts

convert :: Statement -> CoreStatement
convert stmt = case stmt of
  Block loc body                 -> convertBlock loc body
  LabelledStatement loc lab body -> convertLabelled loc lab body
  VarDecl loc decls              -> convertVarDecl loc decls
  ExprStmt loc e                 -> convertExpr loc e
  WhileStatement loc cond body   -> convertWhile loc cond body
  DoWhileStatement loc cond body -> convertDoWhile loc cond body
  For loc header body            -> convertFor loc header body
  IfStatement loc e s1 s2        -> convertIf loc e s1 s2
  ContinueStatement loc label    -> convertContinue loc label
  BreakStatement loc label       -> convertBreak loc label
  Return loc expr                -> convertReturn loc expr
  SwitchStatement loc expr cases -> convertSwitch loc expr cases
  FunDecl loc _ _ _ _            -> convertBlock loc []
  EmptyStatement loc             -> convertBlock loc []
  ThrowStatement loc expr        -> convertThrow loc expr
  WithStatement loc expr body    -> convertWith loc expr body
  TryStatement loc body c f      -> convertTry loc body c f

coreBlock :: [CoreStatement] -> CoreStatement
coreBlock [s] = s
coreBlock xs  = CoreBlock xs

convertBlock :: SrcLoc -> [Statement] -> CoreStatement
convertBlock _loc body = coreBlock (map convert body)

convertLabelled :: SrcLoc -> Label -> Statement -> CoreStatement
convertLabelled loc label body = CoreLabel loc label (convert body)

convertVarDecl :: SrcLoc -> [VarDeclaration] -> CoreStatement
convertVarDecl loc decls = CoreBlock (map makeAssignment . filter (isJust . snd) $ decls)
  where makeAssignment (name, Just val) = CoreExpr loc (Assign (ReadVar name) "=" val)

convertExpr :: SrcLoc -> Expr -> CoreStatement
convertExpr = CoreExpr

convertWhile :: SrcLoc -> Expr -> Statement -> CoreStatement
convertWhile loc expr stmt = CoreLoop loc expr LiteralUndefined (convert stmt)

convertDoWhile :: SrcLoc -> Expr -> Statement -> CoreStatement
convertDoWhile loc expr stmt = CoreBlock [ convert stmt, convertWhile loc expr stmt ]

convertFor :: SrcLoc -> ForHeader -> Statement -> CoreStatement
convertFor loc header stmt = case header of
  For3 e1 e2 e3    -> convertFor3     loc e1 e2 e3 stmt
  For3Var ds e2 e3 -> convertFor3Var  loc ds e2 e3 stmt
  ForIn e1 e2      -> convertForIn    loc e1 e2    stmt
  ForInVar d e     -> convertForInVar loc d  e     stmt

convertFor3 :: SrcLoc -> Maybe Expr -> Maybe Expr -> Maybe Expr -> Statement -> CoreStatement
convertFor3 loc e1 e2 e3 stmt = case e1 of
  Nothing -> CoreLoop loc (maybeExpr e2) (maybeExpr e3) (convert stmt)
  Just e  -> CoreBlock [ CoreExpr loc e, convertFor3 loc Nothing e2 e3 stmt ]

convertFor3Var :: SrcLoc -> [VarDeclaration] -> Maybe Expr -> Maybe Expr -> Statement -> CoreStatement
convertFor3Var loc ds e2 e3 stmt =
  let s1 = convert $ VarDecl loc ds
      s2 = convertFor3 loc Nothing e2 e3 stmt
  in CoreBlock [ s1, s2 ]

convertForIn :: SrcLoc -> Expr -> Expr -> Statement -> CoreStatement
convertForIn loc e1 e2 body = CoreForIn loc e1 e2 (convert body)

convertForInVar :: SrcLoc -> VarDeclaration -> Expr -> Statement -> CoreStatement
convertForInVar loc (var, e1) e2 body =
  let s1 = convert $ VarDecl loc [(var, e1)]
      s2 = convertForIn loc (ReadVar var) e2 body
  in CoreBlock [s1, s2]

convertIf :: SrcLoc -> Expr -> Statement -> Maybe Statement -> CoreStatement
convertIf loc expr s1 s2 = CoreIf loc expr (convert s1) (convert <$> s2)

convertContinue :: SrcLoc -> Maybe Label -> CoreStatement
convertContinue loc label = CoreCont loc label

convertBreak :: SrcLoc -> Maybe Label -> CoreStatement
convertBreak loc label = CoreBreak loc label

convertReturn :: SrcLoc -> Maybe Expr -> CoreStatement
convertReturn loc expr = CoreRet loc (fromMaybe LiteralUndefined expr)

convertThrow :: SrcLoc -> Expr -> CoreStatement
convertThrow = CoreThrow

convertWith :: SrcLoc -> Expr -> Statement -> CoreStatement
convertWith _loc expr stmt = CoreBind DBINone (ObjectEnv expr) [] (convert stmt)

convertSwitch :: SrcLoc -> Expr -> CaseBlock -> CoreStatement
convertSwitch loc e (as, d, bs) = CoreCase loc e cases
  where
    cases = map toCase as ++ dflt ++ map toCase bs
    dflt :: [(Maybe Expr, CoreStatement)]
    dflt  = case d of
      Nothing -> []
      Just (DefaultClause s) -> [(Nothing, CoreBlock $ map convert s)]
    toCase (CaseClause e body) = (Just e, coreBlock $ map convert body)

convertTry :: SrcLoc -> Statement -> Maybe Statement -> Maybe Statement -> CoreStatement
convertTry loc body catch finally =
  CoreTry loc (convert body) (convertCatch <$> catch) (convertFinally <$> finally)
    where convertCatch (Catch _loc var stmt) = (var, convert stmt)
          convertFinally (Finally _loc stmt) = convert stmt


declBindings :: [Statement] -> [(Ident, Expr)]
declBindings stmts = [ (name, LiteralUndefined) | name <- concatMap searchVariables stmts ]
                  ++ concatMap searchFunctions stmts

sourceLocation :: CoreStatement -> SrcLoc
sourceLocation stmt = case stmt of
  CoreBind _ _ _ _ -> error "no srcloc for corebind"
  CoreBlock _      -> error "no srcloc for coreblock"
  CoreExpr a _     -> a
  CoreLoop a _ _ _ -> a
  CoreBreak a _    -> a
  CoreCont a _     -> a
  CoreRet a _      -> a
  CoreCase a _ _   -> a
  CoreIf a _ _ _   -> a
  CoreLabel a _ _  -> a
  CoreTry a _ _ _  -> a




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

