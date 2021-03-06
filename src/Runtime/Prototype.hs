{-# LANGUAGE OverloadedStrings #-}

module Runtime.Prototype where

import Data.Monoid
import Data.Text (Text)

import Runtime.Types
import Runtime.Object
import Runtime.Error
import Runtime.Conversion

makePrototype :: Text -> Runtime (Shared JSObj)
makePrototype name = do
  objectPrototype <- findPrototypeForClass "Object"
  newObject
    >>= setClass name
    >>= objSetPrototype objectPrototype

findPrototypeForClass :: Text -> Runtime (Shared JSObj)
findPrototypeForClass name =
  getGlobalProperty name >>= valGetPrototype >>= maybe oops return
    where
      valGetPrototype (VObj objRef) = fromObj <$> objGet "prototype" objRef
      valGetPrototype _ = return Nothing
      oops = raiseError $ "No prototype for " <> name
