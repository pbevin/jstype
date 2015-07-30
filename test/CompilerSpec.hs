{-# LANGUAGE OverloadedStrings #-}
module CompilerSpec where

import Test.Hspec
import Compiler
import OpCodes
import Runtime
import Expr

spec :: Spec
spec = do
  describe "compileObjectLiteral" $ do
    it "compiles an empty object" $ do
      compileObjectLiteral [] `shouldBe` [ OpObjLit 0 ]

    it "compiles {x:0}" $ do
      straighten (compileObjectLiteral [("x", Value (INum 0))]) `shouldBe`
        [ OpConst (VStr "x"), OpInt 0, OpGetValue, OpObjLit 1 ]
