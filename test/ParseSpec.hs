module ParseSpec where

import Control.Exception (evaluate)
import Test.Hspec
import Test.QuickCheck

import Expr
import Parse

spec = do
  it "parses a number" $ do
    simpleParse "1" `shouldBe` Num (JSNum 1)

  it "parses a negative number" $ do
    simpleParse "-1" `shouldBe` UnOp "-" (Num (JSNum 1))

  it "parses a unary operator" $ do
    simpleParse "++u" `shouldBe` UnOp "++" (ReadVar "u")

  it "parses a unop assignment" $ do
    simpleParse "e = -1" `shouldBe` Assign "e" "=" (UnOp "-" (Num (JSNum 1)))

  it "parses a plus-equals" $ do
    simpleParse "a += b" `shouldBe` Assign "a" "+=" (ReadVar "b")

  it "interprets this tricky case right" $ do
    simpleParse "a++ + ++b" `shouldBe` BinOp "+" (PostOp "++" (ReadVar "a")) (UnOp "++" (ReadVar "b"))
    simpleParse "a+++ ++b" `shouldBe` BinOp "+" (PostOp "++" (ReadVar "a")) (UnOp "++" (ReadVar "b"))
    evaluate (simpleParse "a+++++b") `shouldThrow` anyException
    evaluate (simpleParse "a++ +++b") `shouldThrow` anyException

  it "parses a unop assignment without spaces" $ do
    simpleParse "e=-1" `shouldBe` Assign "e" "=" (UnOp "-" (Num (JSNum 1)))

  it "parses a binop" $ do
    simpleParse "1+2" `shouldBe` BinOp "+" (Num (JSNum 1)) (Num (JSNum 2))
    simpleParse "(1)&&(2)" `shouldBe` BinOp "&&" (Num (JSNum 1)) (Num (JSNum 2))

  it "parses a function call" $ do
    simpleParse "f()" `shouldBe` FunCall (ReadVar "f") []
    simpleParse "f(x)" `shouldBe` FunCall (ReadVar "f") [(ReadVar "x")]
    simpleParse "f(x,y)" `shouldBe` FunCall (ReadVar "f") [(ReadVar "x"), (ReadVar "y")]

  it "is the inverse of showExpr" $ property prop_showExpr
