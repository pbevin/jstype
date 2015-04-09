module Main where

import Test.QuickCheck
import Parse
import Expr

main :: IO ()
main = do
  putStrLn $ show $ simpleParse "a+++++b"
  -- quickCheck prop_showExpr
