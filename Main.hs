{-# LANGUAGE LambdaCase #-}
module Main where

import Control.Monad.Trans
import System.Console.Haskeline
import System.Exit
import System.IO
import System.Environment
import Test.QuickCheck
import Runtime.Conversion (showVal)
import Eval (runJS, evalJS)

main :: IO ()
main = do
  getArgs >>= \case
    [filename] -> runFile filename
    _ -> repl


runFile :: String -> IO ()
runFile filename = do
  input <- readFile filename
  runJS filename input >>= \case
    Right out -> putStr out
    Left err -> hPutStrLn stderr (show err) >> exitFailure


repl :: IO ()
repl = do
  runInputT defaultSettings loop
  where
    loop :: InputT IO ()
    loop = do
      getInputLine "js> " >>= \case
        Nothing     -> return () -- EOF / control-d
        Just "exit" -> return ()
        Just line -> do (liftIO $ process line) >> loop

process :: String -> IO ()
process line = do
  evalJS "(console)" line >>= \case
    Right (Just val) -> putStrLn (showVal val)
    Right Nothing -> return ()
    Left err -> hPutStrLn stderr ("SyntaxError: " ++ show err)
