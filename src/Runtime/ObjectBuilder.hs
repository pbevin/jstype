{-# LANGUAGE GeneralizedNewtypeDeriving, TemplateHaskell, RankNTypes #-}
module Runtime.ObjectBuilder where

import Control.Lens
import Control.Monad.State
import Data.Maybe
import Runtime.Types
import Runtime.Shared
import Runtime.PropertyDescriptor
import Runtime.Adaptable
import Runtime.Object

type PropertyName = String

newtype Builder a = B { unbuild :: StateT JSObj Runtime a }
  deriving (Monad, MonadState JSObj, Applicative, Functor)

liftR :: Runtime a -> Builder a
liftR m = B (lift m)

mkObject :: Builder a -> Runtime (Shared JSObj)
mkObject a = share =<< finish =<< snd <$> runStateT (unbuild a) defaultObject

property :: PropertyName -> JSVal -> Builder ()
property name val = modify $ objSetProperty name val

descriptor :: PropertyName -> PropDesc JSVal -> Builder ()
descriptor name desc = modify $ objSetPropertyDescriptor name desc

constant :: Adaptable a => PropertyName -> a -> Builder ()
constant name val = do
  v <- liftR $ adapt val VUndef []
  descriptor name (dataPD v False False False)

method :: PropertyName -> Int -> JSFunction -> Builder ()
method name arity fn = property name (VNative name arity fn)

native :: Adaptable a => PropertyName -> Int -> a -> Builder ()
native name arity f = liftR (return $ adapt f) >>= method name arity



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
   globalPrototype <- globalObjectPrototype <$> get
   return $ obj & objPrototype .~ globalPrototype

