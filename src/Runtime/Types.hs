{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Runtime.Types where

import Control.Monad.State
import Control.Monad.Except
import Control.Monad.Writer
import Data.IORef
import qualified Data.Map as M
import Control.Applicative

import Expr

data JSVal = VNum JSNum
           | VStr String
           | VBool Bool
           | VRef JSRef
           | VUndef
           | VNull
           | VObj (Shared JSObj)
           | VMap (M.Map Ident JSVal)
           | VNative (JSVal -> [JSVal] -> JSRuntime JSVal)
           | VException JSError
           | VCxt JSCxt

instance Show JSVal where
  show (VNum a) = show a
  show (VStr a) = show a
  show (VBool a) = if a then "true" else "false"
  show (VRef ref) = "(reference " ++ show ref ++ ")"
  show VUndef = "undefined"
  show VNull  = "null"
  show (VObj _) = "[Object object]"
  show (VMap _) = "(map)"
  show (VNative _) = "(native function)"
  show (VException exc) = "Exception " ++ show exc
  show _ = "???"

data JSObj = JSObj {
  objClass :: String,
  ownProperties :: M.Map Ident JSVal,
  callMethod :: Maybe (JSVal -> [JSVal] -> JSRuntime JSVal),
  primitive :: Maybe JSVal
}


data JSRef = JSRef {
  getBase :: JSVal,
  getReferencedName :: String,
  isStrictReference :: Bool
  } deriving Show

data JSCxt = JSCxt {
  lexEnv :: JSEnv,
  varEnv :: JSEnv,
  thisBinding :: JSVal
}

data JSType = TypeUndefined
            | TypeNull
            | TypeBoolean
            | TypeNumber
            | TypeString
            | TypeObject
            | TypeFunction
            | TypeReference
            | TypeList
            | TypeOther String
            deriving (Show, Eq)

typeof :: JSVal -> JSType
typeof v = case v of
  VNum _       -> TypeNumber
  VStr _       -> TypeString
  VBool _      -> TypeBoolean
  VRef  _      -> TypeReference
  VUndef       -> TypeUndefined
  VNull        -> TypeNull
  VNative _    -> TypeFunction
  VObj _       -> TypeObject
  VException _ -> TypeOther "Exception"
  _            -> error $ "No idea what type " ++ show v ++ " is..."

instance Eq JSVal where
  VNum a == VNum b       = a == b
  VStr a == VStr b       = a == b
  VObj a == VObj b       = a == b
  VBool a == VBool b     = a == b
  VNative a == VNative b = True -- XXX
  a == b = error $ "Can't compare " ++ show a ++ " and " ++ show b

type JSOutput = String
type JSError = (String, [SrcLoc])
type JSEnv = Shared (M.Map Ident JSVal)
type JSFunction = JSVal -> [JSVal] -> JSRuntime JSVal

newtype JSGlobal = JSGlobal {
  globalObject :: Maybe (Shared JSObj)
}

newtype Shared a = Shared (IORef a) deriving Eq

data CompletionType = CTNormal | CTBreak | CTContinue | CTReturn | CTThrow deriving (Show, Eq)
type StmtReturn = (CompletionType, Maybe JSVal, Maybe Ident)

share :: a -> JSRuntime (Shared a)
share a = liftM Shared $ liftIO $ newIORef a
deref :: Shared a -> JSRuntime a
deref (Shared a) = liftIO $ readIORef a
modifyRef :: Shared a -> (a -> a) -> JSRuntime ()
modifyRef (Shared a) f = liftIO $ modifyIORef a f
modifyRef' :: Shared a -> (a -> a) -> JSRuntime (Shared a)
modifyRef' a f = modifyRef a f >> return a

newtype JSRuntime a = JS {
  unJS :: ExceptT JSError (WriterT String (StateT JSGlobal IO)) a
} deriving (Monad, MonadIO,
            MonadWriter String,
            MonadError JSError,
            MonadState JSGlobal,
            Functor,
            Applicative)


raiseError :: String -> JSRuntime a
raiseError s = throwError (s, [])

data PrimitiveHint = HintNone | HintNumber
