module ParseSpec where

import Test.Hspec
import Test.QuickCheck

import Expr
import Parse

spec = do
  it "parses a number" $ do
    simpleParse "1" `shouldBe` Num (JSNum 1)
    simpleParse "-1" `shouldBe` UnOp "-" (Num (JSNum 1))

  it "parses a unop assignment" $ do
    simpleParse "e = -1" `shouldBe` Assign "e" "=" (UnOp "-" (Num (JSNum 1)))

  it "parses a unop assignment without spaces" $ do
    simpleParse "e=-1" `shouldBe` Assign "e" "=" (UnOp "-" (Num (JSNum 1)))

  it "is the inverse of showExpr" $ property prop_showExpr
