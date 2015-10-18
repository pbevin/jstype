{-# LANGUAGE LambdaCase #-}
module Main where

import Control.DeepSeq
import Criterion.Main
import qualified Data.Text as T
import Eval (runJS)
import Parse


main = defaultMain [
  bgroup "parse" [ bench "10"   $ nf parseVars 10
                 , bench "100"  $ nf parseVars 100
                 , bench "1000" $ nf parseVars 1000
                 ],
  bgroup "vars"  [ bench "10"   . nfIO $ vars 10
                 , bench "100"  . nfIO $ vars 100
                 , bench "1000" . nfIO $ vars 1000
                 ],
  bgroup "loop"  [ bench "10"   . nfIO $ loop 10
                 , bench "100"  . nfIO $ loop 100
                 , bench "1000" . nfIO $ loop 1000
                 ],
  bgroup "floop" [ bench "10"   . nfIO $ floop 10
                 , bench "100"  . nfIO $ floop 100
                 , bench "1000" . nfIO $ floop 1000
                 ]
  ]

parseVars n = parse . T.pack . unlines . map varDecl $ [1..n]
  where parse input =
          case parseJS' input "" of
            Left err -> show err
            Right ast -> show ast

vars n = run . unlines $ map varDecl [1..n]

varDecl n = "var a" ++ (show n) ++ " = 0;"

loop n = run . unlines $ [ "var i, t=0;"
                         , "for (i = 0; i < " ++ (show n) ++ "; i++) {"
                         , "  t += i;"
                         , "}"
                         ]

floop n = run . unlines $ [ "var i, t=0;"
                          , "for (i = 0; i < " ++ (show n) ++ "; i++) {"
                          , "  t = add(t, i);"
                          , "}"
                          , "function add(a, b) { var t = a + b; return t; }"
                          ]

run prog = runJS "(benchmark)" (T.pack prog)





-- import Control.Monad.Trans
-- import System.Console.Haskeline
-- import System.Exit
-- import System.IO
-- import System.Environment
-- import qualified Data.Text as T
-- import Runtime.Conversion (showVal)
-- import Eval (runJS, evalJS)

-- main :: IO ()
-- main = do
--   getArgs >>= \case
--     [filename] -> runFile filename
--     _ -> repl


-- runFile :: String -> IO ()
-- runFile filename = do
--   input <- readFile filename
--   runJS filename (T.pack input) >>= \case
--     Right out -> putStr out
--     Left (out, err) -> putStr out >> hPutStrLn stderr (show err) >> exitFailure


-- repl :: IO ()
-- repl = do
--   runInputT defaultSettings loop
--   where
--     loop :: InputT IO ()
--     loop = do
--       getInputLine "js> " >>= \case
--         Nothing     -> return () -- EOF / control-d
--         Just "exit" -> return ()
--         Just line -> do (liftIO $ process line) >> loop

-- process :: String -> IO ()
-- process line = do
--   evalJS "(console)" (T.pack line) >>= \case
--     Right (Just val) -> putStrLn . T.unpack $ showVal val
--     Right Nothing -> return ()
--     Left err -> hPutStrLn stderr ("SyntaxError: " ++ show err)
