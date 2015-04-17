module Imports where

import Test.QuickCheck
import Parse
import Expr
import ShowExpr
import Eval
import GenExpr

dbgShrink :: (Code a, Arbitrary a) => a -> IO()
dbgShrink a = putStrLn $ unlines $ map code $ shrink a
