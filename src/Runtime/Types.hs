{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Runtime.Types where

import Control.Monad.State
import Control.Monad.Except
import Control.Monad.Writer
import Text.Show.Functions
import Data.Maybe
import Data.IORef
import Runtime.PropMap
import Control.Applicative
import Expr
import JSNum

type PropertyMap = PropMap Ident (PropDesc JSVal)

data PropDesc a = DataPD a Bool Bool Bool deriving (Show, Eq)
propValue :: PropDesc a -> a
propValue (DataPD a _ _ _) = a

valueToProp :: a -> PropDesc a
valueToProp a = DataPD a True True True

propSetValue :: a -> PropDesc a -> PropDesc a
propSetValue a (DataPD _ w e b) = DataPD a w e b

readOnlyProperty :: a -> PropDesc a
readOnlyProperty a = DataPD a False True True

propIsWritable :: PropDesc a -> Bool
propIsWritable (DataPD _ w _ _) = w

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
  show (VNum a)         = "VNum " ++ show a
  show (VStr a)         = "VStr " ++ show a
  show (VBool a)        = "VBool " ++ if a then "True" else "False"
  show (VRef ref)       = "VRef " ++ show ref
  show VUndef           = "VUndef"
  show VNull            = "VNull"
  show (VObj _)         = "VObj _"
  show (VNative _)      = "VNative _"
  show (VStacktrace st) = "VStacktrace " ++ show st
  show (VEnv env)       = "VEnv " ++ show env

isObj :: JSVal -> Bool
isObj (VObj _) = True
isObj _ = False

isPrimitive :: JSVal -> Bool
isPrimitive VUndef    = True
isPrimitive VNull     = True
isPrimitive (VBool _) = True
isPrimitive (VNum _)  = True
isPrimitive (VStr _)  = True
isPrimitive _         = False

data JSObj = JSObj {
  objClass :: String,
  ownProperties :: PropertyMap,
  objPrototype :: Maybe (Shared JSObj),
  callMethod :: Maybe (JSVal -> [JSVal] -> Runtime JSVal),
  cstrMethod :: Maybe (JSVal -> [JSVal] -> Runtime JSVal)
}

data JSRef = JSRef {
  getBase :: JSVal,
  getReferencedName :: Ident,
  refStrictness :: Strictness
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

data EnvRec = DeclEnvRec (Shared PropertyMap)
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
  m <- share emptyPropMap
  share $ LexEnv (DeclEnvRec m) (Just parent)

type JSOutput = String
data JSError = JSError (JSVal, [SrcLoc]) | JSProtoError (ErrorType, String) deriving (Show, Eq)
type JSFunction = JSVal -> [JSVal] -> Runtime JSVal


newtype Shared a = Shared (IORef a) deriving Eq

instance Show (Shared a) where
  show (Shared _) = "(shared)"

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

runRuntime :: Runtime a -> IO ((Either JSError a, String), JSGlobal)
runRuntime a = runStateT (runWriterT $ runExceptT $ unJS a) emptyGlobal


data JSGlobal = JSGlobal {
  globalObject    :: Maybe (Shared JSObj),
  globalObjectPrototype :: Maybe (Shared JSObj),
  globalEvaluator :: Maybe (String -> Runtime StmtReturn),
  globalRun       :: Maybe ([Statement] -> Runtime StmtReturn),
  globalContext   :: Maybe JSCxt
}

emptyGlobal :: JSGlobal
emptyGlobal = JSGlobal Nothing Nothing Nothing Nothing Nothing


raiseError :: String -> Runtime a
raiseError s = throwError $ JSError (VStr s, [])

raiseProtoError :: ErrorType -> String -> Runtime a
raiseProtoError t msg = throwError $ JSProtoError (t, msg)

data ErrorType = ReferenceError | SyntaxError | TypeError deriving (Show, Eq)

data PrimitiveHint = HintNone | HintNumber | HintString

debug :: Show a => a -> Runtime ()
debug a = do
  liftIO $ print a
