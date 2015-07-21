{-# LANGUAGE GeneralizedNewtypeDeriving, TemplateHaskell, RankNTypes #-}
module Runtime.ObjectBuilder where

import Control.Lens
import Control.Monad.State
import Control.Monad.Reader
import Data.Maybe
import Runtime.Types
import Runtime.Shared
import Runtime.PropertyDescriptor
import Runtime.Function
import Runtime.Object

type PropertyName = String

newtype Builder a = B { unbuild :: StateT JSObj Runtime a }
  deriving (Monad, MonadState JSObj, Applicative, Functor)

liftR :: Runtime a -> Builder a
liftR m = B (lift m)

buildObject :: JSObj -> Builder a -> Runtime JSObj
buildObject obj a = snd <$> runStateT (unbuild a) obj

mkObject :: Builder a -> Runtime (Shared JSObj)
mkObject a = share =<< finish =<< buildObject defaultObject a

updateObject :: Shared JSObj -> Builder a -> Runtime ()
updateObject obj a = do
  o <- deref obj
  newObj <- buildObject o a
  modifyRef obj (const newObj)

overObject :: Shared JSObj -> Builder a -> Runtime ()
overObject obj a = do
  newObj <- fmap snd $ runStateT (unbuild a) =<< deref obj
  modifyRef obj (const newObj)
  return ()

property :: PropertyName -> JSVal -> Builder ()
property name val = modify $ objSetProperty name val

descriptor :: PropertyName -> PropDesc JSVal -> Builder ()
descriptor name desc = modify $ objSetPropertyDescriptor name desc

method :: PropertyName -> Int -> JSFunction -> Builder ()
method name arity fn = property name (VNative name arity fn)

isFunctionObject :: Builder ()
isFunctionObject = do
  functionPrototype <- liftR $ objFindPrototype "Function"
  className "Function"
  prototype functionPrototype
  internal hasInstanceMethod funHasInstance



prototype :: Shared JSObj -> Builder ()
prototype proto = modify $ set objPrototype (Just proto)

internal :: Lens JSObj JSObj (Maybe a) (Maybe a) -> a -> Builder ()
internal l f = modify $ set l (Just f)

onHasInstance :: (Shared JSObj -> JSVal -> Runtime Bool) -> Builder ()
onHasInstance f = modify $ set hasInstanceMethod (Just f)

className :: String -> Builder ()
className name = modify $ set objClass name

extensible :: Builder ()
extensible = modify $ set objExtensible True


finish :: JSObj -> Runtime JSObj
finish obj =
  if isJust (obj^.objPrototype)
  then return obj
  else do
   globalPrototype <- asks globalObjectPrototype
   return $ obj & objPrototype .~ (Just globalPrototype)
