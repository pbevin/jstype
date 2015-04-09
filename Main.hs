module Main where

import Test.QuickCheck
import Parse
import Expr

main :: IO ()
main = do
  quickCheck prop_showExpr
