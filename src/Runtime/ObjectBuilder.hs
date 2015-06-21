{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Runtime.ObjectBuilder where

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
prototype proto = modify $ \obj -> obj { objPrototype = Just proto }

onCall :: JSFunction -> Builder ()
onCall f = modify $ \obj -> obj { callMethod = Just f }

onCstr :: JSFunction -> Builder ()
onCstr f = modify $ \obj -> obj { cstrMethod = Just f }

onHasInstance :: (Shared JSObj -> JSVal -> Runtime Bool) -> Builder ()
onHasInstance f = modify $ \obj -> obj { hasInstanceMethod = Just f }

className :: String -> Builder ()
className name = modify $ \obj -> obj { objClass = name }


finish :: JSObj -> Runtime JSObj
finish obj =
  if isJust (objPrototype obj)
  then return obj
  else do
   globalPrototype <- globalObjectPrototype <$> get
   return $ obj { objPrototype = globalPrototype }
