{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Runtime.Types where

import Control.Monad.State
import Control.Monad.Except
import Control.Monad.Writer
import Data.Maybe
import Data.IORef
import qualified Data.Map as M
import Control.Applicative
import Expr
import JSNum

data JSVal = VNum JSNum
           | VStr String
           | VBool Bool
           | VRef JSRef
           | VUndef
           | VNull
           | VObj (Shared JSObj)
           | VNative (JSVal -> [JSVal] -> Runtime JSVal)
           | VStacktrace [SrcLoc]
           | VEnv JSEnv

instance Show JSVal where
  show (VNum a) = show a
  show (VStr a) = a
  show (VBool a) = if a then "true" else "false"
  show (VRef ref) = "(reference " ++ show ref ++ ")"
  show VUndef = "undefined"
  show VNull  = "null"
  show (VObj _) = "[Object object]"
  show (VNative _) = "(native function)"
  show (VStacktrace st) = "Stacktrace " ++ show st
  show _ = "???"

data JSObj = JSObj {
  objClass :: String,
  ownProperties :: M.Map Ident JSVal,
  callMethod :: Maybe (JSVal -> [JSVal] -> Runtime JSVal),
  cstrMethod :: Maybe (JSVal -> [JSVal] -> Runtime JSVal),
  primitive :: Maybe JSVal
}


data JSRef = JSRef {
  getBase :: JSVal,
  getReferencedName :: Ident,
  strictness :: Strictness
} deriving Show

data JSCxt = JSCxt {
  lexEnv :: JSEnv,
  varEnv :: JSEnv, -- never changes, might not need to be shared?
  thisBinding :: JSVal,
  cxtStrictness :: Strictness
}

type JSEnv = Shared LexEnv

data LexEnv = LexEnv {
  envRec :: EnvRec,
  outer  :: Maybe JSEnv
}

data EnvRec = DeclEnvRec (Shared (M.Map Ident JSVal))
            | ObjEnvRec (Shared JSObj)

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
  VNum _        -> TypeNumber
  VStr _        -> TypeString
  VBool _       -> TypeBoolean
  VRef  _       -> TypeReference
  VUndef        -> TypeUndefined
  VNull         -> TypeNull
  VNative _     -> TypeFunction
  VObj _        -> TypeObject
  VStacktrace _ -> TypeOther "Stacktrace"
  _             -> error $ "No idea what type " ++ show v ++ " is..."

instance Eq JSVal where
  VNum a == VNum b       = a == b
  VStr a == VStr b       = a == b
  VObj a == VObj b       = a == b
  VBool a == VBool b     = a == b
  VNative _ == VNative _ = True -- XXX
  _a == _b = False



newEnv :: JSEnv -> Runtime JSEnv
newEnv parent = do
  m <- share M.empty
  share $ LexEnv (DeclEnvRec m) (Just parent)

type JSOutput = String
type JSError = (JSVal, [SrcLoc])
type JSFunction = JSVal -> [JSVal] -> Runtime JSVal


newtype Shared a = Shared (IORef a) deriving Eq

data CompletionType = CTNormal | CTBreak | CTContinue | CTReturn | CTThrow deriving (Show, Eq)
type StmtReturn = (CompletionType, Maybe JSVal, Maybe Ident)

share :: a -> Runtime (Shared a)
share a = liftM Shared $ liftIO $ newIORef a
deref :: Shared a -> Runtime a
deref (Shared a) = liftIO $ readIORef a
modifyRef :: Shared a -> (a -> a) -> Runtime ()
modifyRef (Shared a) f = liftIO $ modifyIORef a f
modifyRef' :: Shared a -> (a -> a) -> Runtime (Shared a)
modifyRef' a f = modifyRef a f >> return a

newtype Runtime a = JS {
  unJS :: ExceptT JSError (WriterT String (StateT JSGlobal IO)) a
} deriving (Monad, MonadIO,
            MonadWriter String,
            MonadError JSError,
            MonadState JSGlobal,
            Functor,
            Applicative)


data JSGlobal = JSGlobal {
  globalObject    :: Maybe (Shared JSObj),
  globalObjectPrototype :: Maybe (Shared JSObj),
  globalEvaluator :: Maybe (String -> Runtime StmtReturn),
  globalRun       :: Maybe ([Statement] -> Runtime StmtReturn),
  globalContext   :: Maybe JSCxt
}

raiseError :: String -> Runtime a
raiseError s = throwError (VStr s, [])

data PrimitiveHint = HintNone | HintNumber
