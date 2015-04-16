module ShowExprSpec where

import Test.Hspec
import Test.QuickCheck

import Eval
import Expr
import ShowExpr

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
    let a = IfStatement (ReadVar "a")
                        (IfStatement (ReadVar "b")
                                     ContinueStatement
                                     (Just BreakStatement))
                        Nothing
    let b = IfStatement (ReadVar "a")
                        (IfStatement (ReadVar "b")
                                     ContinueStatement
                                     Nothing)
                        (Just BreakStatement)

    showProg (Program [a]) `shouldBe`
      "if (a) {if (b) {continue} else {break}}"
    showProg (Program [b]) `shouldBe`
      "if (a) {if (b) {continue}} else {break}"

  it "parenthesizes {} when needed" $ do
    -- {} could mean a block in some cases.
    let empty = ObjectLiteral []
    showProg (Program [ExprStmt empty]) `shouldBe`
      "({})"
    showProg (Program [VarDecl [("a", Just $ empty)]]) `shouldBe`
      "var a = {}"
