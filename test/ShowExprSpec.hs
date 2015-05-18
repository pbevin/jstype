module ShowExprSpec where

import Test.Hspec
import Test.QuickCheck

import Eval
import Expr
import ShowExpr

s = SrcLoc "" 0 0 Nothing

spec :: Spec
spec = do
  it "shows a number" $ do
    showExpr (Num 1.5) `shouldBe` "1.5"
    showExpr (Num 1) `shouldBe` "1"

  it "shows a unary op" $ do
    showExpr (UnOp "-" (Num 1)) `shouldBe` "-1"
    showExpr (UnOp "-" (ReadVar "a")) `shouldBe` "-a"
    showExpr (UnOp "-" (BinOp "+" (ReadVar "a") (ReadVar "b"))) `shouldBe` "-(a + b)"

  it "shows a function call" $ do
    showExpr (FunCall (ReadVar "f") [ReadVar "x"]) `shouldBe` "f(x)"
    showExpr (FunCall (BinOp "||" (ReadVar "f") (ReadVar "g")) [ReadVar "x"]) `shouldBe` "(f || g)(x)"

  it "shows ambiguous if-then-else statements correctly" $ do
    let a = IfStatement s (ReadVar "a")
                        (IfStatement s (ReadVar "b")
                                     (ContinueStatement s Nothing)
                                     (Just $ BreakStatement s Nothing))
                        Nothing
    let b = IfStatement s (ReadVar "a")
                        (IfStatement s (ReadVar "b")
                                     (ContinueStatement s Nothing)
                                     Nothing)
                        (Just $ BreakStatement s Nothing)

    showProg (Program [a]) `shouldBe`
      "if (a) {if (b) {continue} else {break}}"
    showProg (Program [b]) `shouldBe`
      "if (a) {if (b) {continue}} else {break}"

  it "parenthesizes {} when needed" $ do
    -- {} could mean a block in some cases.
    let empty = ObjectLiteral []
    showProg (Program [ExprStmt s empty]) `shouldBe`
      "({})"
    showProg (Program [VarDecl s [("a", Just $ empty)]]) `shouldBe`
      "var a = {}"
