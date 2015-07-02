{-# LANGUAGE LambdaCase #-}

module Builtins.Array.Reduce (arrayReduce) where

import Control.Monad (foldM, when)
import Data.Maybe
import Safe
import Runtime

mustBeCallable :: JSVal -> Runtime JSFunction
mustBeCallable val = isCallable val >>= \case
  Nothing -> raiseTypeError $ showVal val ++ " is not a function"
  Just fn -> return fn

-- ref 15.4.4.21, incomplete
arrayReduce :: JSFunction
arrayReduce this args = do
  let callbackfn   = first1 args
      initialValue = args `atMay` 1

  o <- toObject this
  len <- fromIntegral <$> (toInt32 =<< objGet "length" o)
  cb <- mustBeCallable callbackfn

  when (len == 0 && initialValue == Nothing) $
    raiseTypeError "Reduce of empty array with no initial value"

  val <- foldM (fn cb o) initialValue [0..len-1]
  case val of
    Nothing -> raiseTypeError "Value not found and no default"
    Just v  -> return v

  where
    fn :: JSFunction -> Shared JSObj -> Maybe JSVal -> Int -> Runtime (Maybe JSVal)
    fn callback obj acc k = do
      val <- atIndex k obj
      case (val, acc) of
        (Nothing, _)      -> return acc
        (Just _, Nothing) -> return val
        (Just y, Just x)  -> Just <$> callback VUndef [x, y, VNum $ fromIntegral k, VObj obj]

atIndex :: Int -> Shared JSObj -> Runtime (Maybe JSVal)
atIndex k obj = do
    exists <- objHasProperty (show k) obj
    if exists
    then Just <$> objGet (show k) obj
    else return Nothing
