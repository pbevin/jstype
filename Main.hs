module Main where

import System.Exit
import System.IO
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
  result <- runJS filename input
  case result of
    Right output -> putStr output
    Left err -> hPutStrLn stderr (show err) >> exitFailure
