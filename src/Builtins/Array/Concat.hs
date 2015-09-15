{-# LANGUAGE OverloadedStrings #-}

module Builtins.Array.Concat (arrayConcat) where

import qualified Data.Text as T
import Control.Lens (view)
import Runtime
import Runtime.Debug

-- ref 15.4.4.4
arrayConcat :: JSFunction
arrayConcat this args = do
  o <- toObject this
  createArray =<< concat <$> mapM getElements (VObj o : args)

getElements :: JSVal -> Runtime [Maybe JSVal]
getElements v@(VObj obj) = do
  cls <- view objClass <$> deref obj
  if cls == "Array"
     then exportArray obj
     else return [Just v]
getElements v = return [Just v]

exportArray :: Shared JSObj -> Runtime [Maybe JSVal]
exportArray obj = do
  -- debugVal (VObj obj)
  VInt len <- objGet "length" obj
  mapM (getOne obj) [0..len-1]

getOne :: Shared JSObj -> Integer -> Runtime (Maybe JSVal)
getOne obj n =
  let p = T.pack (show n)
   in do exists <- objHasProperty p obj
         if exists
            then Just <$> objGet p obj
            else return Nothing
