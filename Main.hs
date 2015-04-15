module Main where

import System.Environment
import Test.QuickCheck
import Parse
import Expr
import Eval
import ShowExpr

main :: IO ()
main = do
  args <- getArgs
  case args of
    [filename] -> runFile filename
    _ -> error "Usage"


runFile :: String -> IO ()
runFile filename = do
  input <- readFile filename
  case jsEval input filename of
    Just output -> putStr output
    Nothing -> return ()
