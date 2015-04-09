module ParseSpec where

import Test.Hspec
import Test.QuickCheck

import Expr
import Parse

spec = do
  it "is the inverse of showExpr" $ property prop_showExpr
