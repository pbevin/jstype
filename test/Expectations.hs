{-# LANGUAGE LambdaCase #-}

module Expectations where

import Test.Hspec
import Runtime
import Eval

shouldBeError :: Show a => Either JSError a -> ErrorType -> Expectation
shouldBeError (Left err) errType = case err of
  JSProtoError (et, _) -> et `shouldBe` errType
  _ -> expectationFailure $ show err
shouldBeError (Right val) errType = expectationFailure $ "was a val (" ++ show val ++ "), not an error of type " ++ show errType

shouldError :: Show b => IO (Either RuntimeError b) -> String -> Expectation
shouldError val errorText = val >>= \case
  Left err -> errorMessage err `shouldBe` errorText
  Right v  -> expectationFailure $ "was a val (" ++ show v ++ "), not an error"
