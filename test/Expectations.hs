{-# LANGUAGE LambdaCase #-}

module Expectations where

import Control.Monad.Trans (liftIO)
import Test.Hspec
import Data.List (isPrefixOf)
import Runtime
import Eval

shouldBeError :: Show a => Either JSError a -> ErrorType -> Expectation
shouldBeError (Left err) errType = case err of
  JSProtoError (et, _) -> et `shouldBe` errType
  _ -> expectationFailure $ show err
shouldBeError (Right val) errType = expectationFailure $ "was a val (" ++ show val ++ "), not an error of type " ++ show errType

shouldBeResult :: (Show a, Eq a) => Either JSError a -> a -> Expectation
shouldBeResult (Left err) _ = expectationFailure $ "was an error (" ++ show err ++ ")"
shouldBeResult (Right v) val = val `shouldBe` v

shouldReturnResult :: (Show a, Eq a) => IO (Either JSError a) -> a -> Expectation
shouldReturnResult p val = do
  result <- p
  result `shouldBeResult` val

shouldError :: Show b => IO (Either RuntimeError b) -> String -> Expectation
shouldError val errorText = val >>= \case
  Left err -> errorMessage err `shouldBe` errorText
  Right v  -> expectationFailure $ "was a val (" ++ show v ++ "), not an error"

shouldErrorOfType :: Show b => IO (Either RuntimeError b) -> ErrorType -> Expectation
shouldErrorOfType val errorType = val >>= \case
  Left err -> errorMessage err `shouldStartWith` (show errorType ++ ":")
  Right v  -> expectationFailure $ "was a val (" ++ show v ++ "), not an error"

shouldStartWith :: String -> String -> Expectation
shouldStartWith haystack needle
  | needle `isPrefixOf` haystack = return ()
  | otherwise = expectationFailure $
      "Expected string starting with " ++ show needle ++ ", got " ++ show haystack

runJStr :: String -> IO (Either RuntimeError String)
runJStr = runJS ""

unObj :: JSVal -> Shared JSObj
unObj (VObj obj) = obj

arrayLength :: [Maybe JSVal] -> IO (Either JSError JSVal)
arrayLength xs = runtime' $ do
  arr <- createArray xs
  objGet "length" (unObj arr)

runjs :: Runtime a -> IO ()
runjs a = runtime a >> return ()

jsEval :: String -> Runtime JSVal
jsEval = liftIO . jsEvalExpr
