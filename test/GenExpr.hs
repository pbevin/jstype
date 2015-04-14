module GenExpr where

import Control.Applicative
import Control.Monad

import Test.QuickCheck
import Expr

instance Arbitrary Program where
  arbitrary = sized arbProg
  shrink = shrinkProg

instance Arbitrary Statement where
  arbitrary = sized arbStmt
  shrink = shrinkStmt

instance Arbitrary Expr where
  arbitrary = sized arbExpr
  shrink = shrinkExpr

arbProg :: Int -> Gen Program
arbProg n = Program <$> shortListOf n arbitrary

arbStmt :: Int -> Gen Statement
arbStmt 0 = ReturnStatement <$> resize 0 arbitrary

arbStmt n = oneof [ ExprStmt <$> resize (n-1) arbitrary,
                    WhileStatement <$> arbExpr half <*> arbStmt half,
                    ReturnStatement <$> resize (n-1) arbitrary,
                    VarDecl <$> shortListOf1 n (sized arbVarDecl),
                    IfStatement <$> subexpr <*> substmt <*> pure Nothing,
                    IfStatement <$> (resize third arbitrary) <*> (resize third arbitrary) <*> (Just <$> resize third arbitrary),
                    Block <$> resize (n-1) arbitrary,
                    pure EmptyStatement,
                    pure DebuggerStatement ]
  where half = n `div` 2
        third = n `div` 3
        subexpr = resize half arbitrary
        substmt = resize half arbitrary


arbVarDecl :: Int -> Gen (Ident, Maybe Expr)
arbVarDecl n = do
  id <- arbIdent
  expr <- oneof [pure Nothing, Just <$> arbExpr n]
  return (id, expr)






arbExpr :: Int -> Gen Expr
arbExpr 0 = oneof [ Num <$> arbNum,
                    Str <$> arbitrary,
                    ReadVar <$> arbIdent ]
arbExpr n = oneof [ BinOp <$> arbOp <*> subexpr <*> subexpr,
                    UnOp <$> arbUnary <*> resize (n-1) arbitrary,
                    PostOp <$> arbPostfix <*> subexpr,
                    pure This,
                    NewExpr <$> subexpr <*> shortListOf half arbitrary,
                    Assign <$> (ReadVar <$> arbIdent) <*> arbAssignOp <*> resize (n-1) arbitrary,
                    Assign <$> (MemberDot <$> (ReadVar <$> arbIdent) <*> arbIdent)
                           <*> arbAssignOp
                           <*> resize (n-1) arbitrary,
                    FunCall <$> subexpr <*> shortListOf half arbitrary,
                    MemberDot <$> arbitrary <*> arbIdent,
                    MemberGet <$> subexpr <*> subexpr,
                    FunDef Nothing <$> listOf arbIdent <*> pure [],
                    FunDef <$>
                      (Just <$> arbIdent) <*>
                      listOf arbIdent <*>
                      shortListOf n arbitrary,
                    Cond <$> subexpr3 <*> subexpr3 <*> subexpr3 ]
  where subexpr = resize half arbitrary
        subexpr3 = resize third arbitrary
        half = (n-1) `div` 2
        third = (n-1) `div` 3

arbOp :: Gen String
arbOp = elements (binaryOps jsLang)

arbUnary :: Gen String
arbUnary = elements (unaryOps jsLang)

arbPostfix :: Gen String
arbPostfix = elements (postfixOps jsLang)

arbNum :: Gen JSNum
arbNum = arbitrary >>= return . JSNum . getNonNegative

arbAssignOp :: Gen String
arbAssignOp = elements $ assignOps jsLang

arbIdent :: Gen Ident
arbIdent = arbVarName `suchThat` notReserved
  where notReserved name = not $ name `elem` reservedWords jsLang

arbVarName :: Gen Ident
arbVarName = do
  x <- elements ['a'..'z']
  xs <- listOf $ elements $ ['a'..'z'] ++ ['0'..'9'] ++ ['A'..'Z'] ++ ['_']
  return (x:xs)

shortListOf :: Int -> Gen a -> Gen [a]
shortListOf n a = do
  let top = floor (sqrt $ fromIntegral n :: Double)
  len <- choose(0, top `max` 1)
  vectorOf len $ resize top a

shortListOf1 :: Int -> Gen a -> Gen [a]
shortListOf1 n a = do
  let top = floor (sqrt $ fromIntegral n :: Double)
  len <- choose(1, top `max` 1)
  vectorOf len $ resize top a


shrinkProg :: Program -> [Program]
shrinkProg (Program stmts) = [Program p | p <- recursivelyShrink stmts]

shrinkStmt :: Statement -> [Statement]
shrinkStmt expr = case expr of
  ExprStmt e ->
    [ExprStmt e | e <- shrink e]

  WhileStatement e s ->
    [ExprStmt e, s] ++ [WhileStatement e' s' | e' <- shrink e, s' <- shrink s]

  ReturnStatement e ->
    [ReturnStatement e' | e' <- shrinkExpr e]

  IfStatement a b Nothing ->
    [IfStatement a' b' Nothing | a' <- shrink a, b' <- shrink b]

  IfStatement a b c ->
    [IfStatement a' b' c' | (a', b', c') <- shrink (a, b, c)]

  EmptyStatement -> []

  _ -> [EmptyStatement]


shrinkExpr :: Expr -> [Expr]
shrinkExpr expr = case expr of
  Str "" -> []
  Str _ -> [Str ""]
  ReadVar [ch] -> []
  ReadVar (ch:_) -> [ReadVar [ch]]
  Num (JSNum n)
    | n == 0 -> []
    | otherwise -> [Num (JSNum 0)]

  BinOp op e1 e2 ->
    [e1, e2] ++ [BinOp op e1' e2' | e1' <- shrinkExpr e1, e2' <- shrinkExpr e2]

  UnOp op e ->
    [e] ++ [UnOp op e' | e' <- shrinkExpr e]

  PostOp op e ->
    [e] ++ [PostOp op e' | e' <- shrinkExpr e]

  Assign lhs op rhs ->
    [Assign lhs' op rhs' | lhs' <- shrinkExpr lhs, rhs' <- shrinkExpr rhs] ++ [rhs]

  MemberDot e id ->
    [e] ++ [MemberDot e' id | e' <- shrink e]

  MemberGet a x ->
    [a, x] ++ [MemberGet a' x' | a' <- shrink a, x' <- shrink x]

  Cond a b c ->
    [a, b, c] ++ [Cond a' b' c' | a' <- shrinkExpr a, b' <- shrinkExpr b, c' <- shrinkExpr c]

  FunCall f xs ->
    [f] ++ xs ++
      [FunCall f' xs | f' <- shrinkExpr f] ++
      [FunCall f xs' | xs' <- shrinkList shrinkExpr xs]

  FunDef name params body ->
    [ FunDef (Just "f") [] body' | body' <- shrinkList shrinkStmt body ] ++
      [ FunDef name params body' | body' <- shrinkList shrinkStmt body ]
