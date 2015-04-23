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

instance Arbitrary ForHeader where
  arbitrary = arbForHeader
  shrink = shrinkForHeader

instance Arbitrary PropertyName where
  arbitrary = sized arbPropertyName
  shrink = shrinkPropertyName

arbProg :: Int -> Gen Program
arbProg n = Program <$> shortListOf n arbitrary

arbStmt :: Int -> Gen Statement
arbStmt 0 = Return <$> resize 0 arbitrary

arbStmt n = oneof [ ExprStmt <$> resize (n-1) arbitrary,
                    WhileStatement <$> arbExpr half <*> arbStmt half,
                    Return <$> resize (n-1) arbitrary,
                    VarDecl <$> shortListOf1 n (sized arbVarDecl),
                    IfStatement <$> subexpr <*> substmt <*> pure Nothing,
                    IfStatement <$> (resize third arbitrary) <*> (resize third arbitrary) <*> (Just <$> resize third arbitrary),
                    For <$> arbitrary <*> arbitrary,
                    Block <$> two (resize half arbitrary),
                    ThrowStatement <$> arbitrary,
                    TryStatement <$> (block half) <*> (Just <$> Catch "e" <$> block half) <*> pure Nothing,
                    TryStatement <$> (block half) <*> pure Nothing <*> (Just <$> Finally <$> block half),
                    pure ContinueStatement,
                    pure BreakStatement,
                    pure EmptyStatement,
                    pure DebuggerStatement ]
  where half = n `div` 2
        third = n `div` 3
        subexpr = resize half arbitrary
        substmt = resize half arbitrary
        block n = Block <$> shortListOf n arbitrary


arbVarDecl :: Int -> Gen (Ident, Maybe Expr)
arbVarDecl n = do
  id <- arbIdent
  expr <- oneof [pure Nothing, Just <$> arbExpr n]
  return (id, expr)


two :: Gen a -> Gen [a]
two = vectorOf 2



arbExpr :: Int -> Gen Expr
arbExpr 0 = oneof [ Num <$> arbNum,
                    Str <$> arbitrary,
                    ReadVar <$> arbIdent,
                    ArrayLiteral <$> pure [] ]
arbExpr n = oneof [ BinOp <$> arbOp <*> subexpr <*> subexpr,
                    UnOp <$> arbUnary <*> resize (n-1) arbitrary,
                    PostOp <$> arbPostfix <*> subexpr,
                    ArrayLiteral <$> shortListOf (n-1) arbitrary,
                    ObjectLiteral <$> shortListOf (n-1) arbitrary,
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

arbForHeader :: Gen ForHeader
arbForHeader = oneof [ For3 <$> arbitrary <*> arbitrary <*> arbitrary,
                       ForIn <$> (ReadVar <$> arbIdent) <*> arbitrary ]

arbPropertyName :: Int -> Gen PropertyName
arbPropertyName n = oneof [ IdentProp <$> arbIdent,
                            StringProp <$> arbitrary,
                            NumProp <$> arbIndex ]

arbIndex :: Gen JSNum
arbIndex = do
  d <- (arbitrary :: Gen (Positive Integer))
  return $ JSNum $ fromIntegral $ getPositive $ d

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
    [ExprStmt e, s] ++ [WhileStatement e' s' | (e', s') <- shrink (e, s)]

  Return e ->
    [Return e' | e' <- shrink e]

  IfStatement a b Nothing ->
    [IfStatement a' b' Nothing | (a', b') <- shrink (a, b)]

  IfStatement a b c ->
    [IfStatement a' b' c' | (a', b', c') <- shrink (a, b, c)]

  EmptyStatement -> []

  For header stmt ->
    [For h' s' | (h', s') <- shrink (header, stmt)]

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
    shrink e1 ++ shrink e2 ++ [BinOp op e1' e2' | (e1', e2') <- shrinkPair (e1, e2)]

  ArrayLiteral exprs ->
    [ArrayLiteral exprs' | exprs' <- shrinkList shrink exprs] ++ exprs

  ObjectLiteral assigns ->
    [ObjectLiteral a' | a' <- shrinkList shrink assigns] ++ map snd assigns

  UnOp op e ->
    shrink e ++ [UnOp op e' | e' <- shrink e]

  PostOp op e ->
    shrink e ++ [PostOp op e' | e' <- shrink e]

  Assign lhs op rhs ->
    [Assign lhs' op rhs' | (lhs', rhs') <- shrinkPair (lhs, rhs)] ++ shrink rhs

  MemberDot e id ->
    shrink e ++ [MemberDot e' id | e' <- shrink e]

  MemberGet a x ->
    shrink a ++ shrink x ++ [MemberGet a' x' | (a', x') <- shrinkPair (a, x)]

  Cond a b c ->
    [a, b, c] ++ [Cond a' b' c' | (a', b', c') <- shrink (a, b, c)]

  FunCall f xs ->
    [f] ++ xs ++
      [FunCall f' xs | f' <- shrink f] ++ -- XXX
      [FunCall f xs' | xs' <- shrinkList shrink xs]

  FunDef name params body ->
    [ FunDef (Just "f") [] body' | body' <- shrinkList shrinkStmt body ] ++
      [ FunDef name params body' | body' <- shrinkList shrinkStmt body ]

  NewExpr f args ->
    args ++ [NewExpr f' args' | (f', args') <- shrink (f, args)]

shrinkPair :: (Arbitrary a, Arbitrary b) => (a, b) -> [(a, b)]
shrinkPair (a, b) = [(a', b') | a' <- shrink a, b' <- shrink b]

shrinkForHeader :: ForHeader -> [ForHeader]
shrinkForHeader header = case header of
  For3 a b c -> [For3 a' b' c' | (a', b', c') <- shrink (a,b,c)]
  ForIn a b -> [ForIn a' b' | (a', b') <- shrink (a,b)]

shrinkPropertyName :: PropertyName -> [PropertyName]
shrinkPropertyName _ = []
