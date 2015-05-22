module Imports where

import Test.QuickCheck
import Parse
import Expr
import Eval

dbgShrink :: (Code a, Arbitrary a) => a -> IO()
dbgShrink a = putStrLn $ unlines $ map code $ shrink a
