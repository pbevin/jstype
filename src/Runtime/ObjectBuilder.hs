{-# LANGUAGE GeneralizedNewtypeDeriving, TemplateHaskell #-}
module Runtime.ObjectBuilder where

import Control.Lens
import Control.Monad.State
import Data.Maybe
import Runtime.Types
import Runtime.Shared
import Runtime.Object

type PropertyName = String

newtype Builder a = B { unbuild :: StateT JSObj Runtime a }
  deriving (Monad, MonadState JSObj, Applicative, Functor)

mkObject :: Builder a -> Runtime (Shared JSObj)
mkObject a = share =<< finish =<< snd <$> runStateT (unbuild a) defaultObject

property :: PropertyName -> JSVal -> Builder ()
property name val = modify $ objSetProperty name val

method :: PropertyName -> Int -> JSFunction -> Builder ()
method name arity fn = property name (VNative name arity fn)

prototype :: Shared JSObj -> Builder ()
prototype proto = modify $ set objPrototype (Just proto)

onCall :: JSFunction -> Builder ()
onCall f = modify $ set callMethod (Just f)

onCstr :: JSFunction -> Builder ()
onCstr f = modify $ set cstrMethod (Just f)

onHasInstance :: (Shared JSObj -> JSVal -> Runtime Bool) -> Builder ()
onHasInstance f = modify $ set hasInstanceMethod (Just f)

className :: String -> Builder ()
className name = modify $ set objClass name


finish :: JSObj -> Runtime JSObj
finish obj =
  if isJust (obj^.objPrototype)
  then return obj
  else do
   globalPrototype <- globalObjectPrototype <$> get
   return $ obj & objPrototype .~ globalPrototype

