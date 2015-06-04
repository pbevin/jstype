{-# LANGUAGE GeneralizedNewtypeDeriving, LambdaCase #-}

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

data PropDesc a = DataPD a Bool Bool Bool
                | AccessorPD (Maybe (JSVal -> Runtime a)) (Maybe (a -> Runtime ())) Bool Bool

instance Show a => Show (PropDesc a) where
  show (DataPD a w e c) = unwords [ "DataPD", show a, show w, show e, show c ]
  show (AccessorPD g s e c) = unwords [ "AccessorPD", show $ isJust g, show $ isJust s, show e, show c ]

propValue :: PropDesc a -> JSVal -> Runtime a
propValue (DataPD a _ _ _) _ = return a
propValue (AccessorPD (Just getter) _ _ _) this = getter this
propValue (AccessorPD Nothing _ _ _) _ = raiseProtoError ReferenceError "No value"

propIsWritable :: PropDesc a -> Bool
propIsWritable (DataPD _ w _ _) = w
propIsWritable (AccessorPD _ (Just _) _ _) = True
propIsWritable (AccessorPD _ Nothing _ _) = False

propIsEnumerable :: PropDesc a -> Bool
propIsEnumerable (DataPD _ _ e _) = e
propIsEnumerable (AccessorPD _ _ e _) = e

propIsConfigurable :: PropDesc a -> Bool
propIsConfigurable (DataPD _ _ _ c) = c
propIsConfigurable (AccessorPD _ _ _ c) = c

valueToProp :: a -> PropDesc a
valueToProp a = DataPD a True True True

propCopyValue :: PropDesc a -> PropDesc a -> PropDesc a
propCopyValue (DataPD a _ _ _) (DataPD _ w e c) = DataPD a w e c

propSetValue :: a -> PropDesc a -> PropDesc a
propSetValue a (DataPD _ w e c) = DataPD a w e c

readOnlyProperty :: a -> PropDesc a
readOnlyProperty a = DataPD a False True True

isDataDescriptor :: PropDesc a -> Bool
isDataDescriptor DataPD {} = True
isDataDescriptor AccessorPD {} = False

isAccessorDescriptor :: PropDesc a -> Bool
isAccessorDescriptor DataPD {} = False
isAccessorDescriptor AccessorPD {} = True

hasGetter :: PropDesc a -> Bool
hasGetter DataPD {} = False
hasGetter (AccessorPD Nothing _ _ _) = False
hasGetter _ = True

hasSetter :: PropDesc a -> Bool
hasSetter DataPD {} = False
hasSetter (AccessorPD _ Nothing _ _) = False
hasSetter _ = True


data JSVal = VNum JSNum
           | VStr String
           | VBool Bool
           | VRef JSRef
           | VUndef
           | VNull
           | VObj (Shared JSObj)
           | VNative (JSVal -> [JSVal] -> Runtime JSVal)
           | VStacktrace [SrcLoc]
           | VEnv EnvRec

instance Show JSVal where
  show (VNum a)         = "VNum " ++ show a
  show (VStr a)         = "VStr " ++ show a
  show (VBool a)        = "VBool " ++ if a then "True" else "False"
  show (VRef ref)       = "VRef " ++ show ref
  show VUndef           = "VUndef"
  show VNull            = "VNull"
  show (VObj ref)       = "VObj " ++ show ref
  show (VNative _)      = "VNative _"
  show (VStacktrace st) = "VStacktrace " ++ show st
  show (VEnv env)       = "VEnv " ++ show env

isObj :: JSVal -> Bool
isObj (VObj _) = True
isObj _ = False

toObj :: JSVal -> Shared JSObj
toObj = fromJust . fromObj

fromObj :: JSVal -> Maybe (Shared JSObj)
fromObj (VObj v) = Just v
fromObj _ = Nothing

ifUndefined :: JSVal -> JSVal -> JSVal
ifUndefined dflt VUndef = dflt
ifUndefined _ val       = val

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
  callMethod :: Maybe JSFunction,
  cstrMethod :: Maybe JSFunction,
  hasInstanceMethod :: Maybe (Shared JSObj -> JSVal -> Runtime Bool),
  defineOwnPropertyMethod :: Maybe (String -> PropDesc JSVal -> Bool -> Shared JSObj -> Runtime (Shared JSObj)),
  objPrimitiveValue :: Maybe JSVal,
  objParameterMap :: Maybe JSVal,
  objExtensible :: Bool
} deriving Show

emptyObject :: JSObj
emptyObject = JSObj {
  objClass = "Object",
  ownProperties = emptyPropMap,
  objPrototype = Nothing,
  callMethod = Nothing,
  cstrMethod = Nothing,
  hasInstanceMethod = Nothing,
  defineOwnPropertyMethod = Nothing,
  objPrimitiveValue = Nothing,
  objParameterMap = Nothing,
  objExtensible = True
}

objDefineOwnProperty :: String -> PropDesc JSVal -> Bool -> Shared JSObj -> Runtime (Shared JSObj)
objDefineOwnProperty name desc strict objRef = (defineOwnPropertyMethod <$> deref objRef) >>= \case
  Nothing -> raiseProtoError ReferenceError "No defineOwnProperty method"
  Just m -> m name desc strict objRef

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

type JSEnv = Shared LexEnv -- XXX make this just LexEnv so it's obvious when shared

data LexEnv = LexEnv {
  envRec :: EnvRec,
  outer  :: Maybe JSEnv
} deriving Show

data EnvRec = DeclEnvRec (Shared PropertyMap)
            | ObjEnvRec (Shared JSObj) Bool -- Bool = provideThis
            deriving (Show, Eq)

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



newEnv :: EnvRec -> JSEnv -> Runtime JSEnv
newEnv rec parent = do
  share $ LexEnv rec (Just parent)

newEnvRec :: Runtime EnvRec
newEnvRec = do
  m <- share emptyPropMap
  return (DeclEnvRec m)

type JSOutput = String
data JSError = JSError (JSVal, [SrcLoc]) | JSProtoError (ErrorType, String) deriving (Show, Eq)
type JSFunction = JSVal -> [JSVal] -> Runtime JSVal

data CompletionType = CTNormal | CTBreak | CTContinue | CTReturn | CTThrow deriving (Show, Eq)
type StmtReturn = (CompletionType, Maybe JSVal, Maybe Ident)


-- Shared type
type ObjID = Int
data Shared a = Shared (IORef a) ObjID deriving Eq

instance Show (Shared a) where
  show (Shared _ objId) = "(shared #" ++ show objId ++ ")"

nextID :: Runtime Int
nextID = do
  global <- get
  let objId = 1 + globalNextID global
  put global { globalNextID = objId }
  return objId

share :: Show a => a -> Runtime (Shared a)
share a = do
  objId <- nextID
  Shared <$> liftIO (newIORef a) <*> pure objId
deref :: Shared a -> Runtime a
deref (Shared a _) = liftIO $ readIORef a
modifyRef :: Shared a -> (a -> a) -> Runtime ()
modifyRef (Shared a _) f = liftIO $ modifyIORef a f
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
  globalNextID    :: Int,
  globalObject    :: Maybe (Shared JSObj),
  globalObjectPrototype :: Maybe (Shared JSObj),
  globalEvaluator :: Maybe (String -> Runtime StmtReturn),
  globalRun       :: Maybe ([Statement] -> Runtime StmtReturn),
  globalContext   :: Maybe JSCxt
}

emptyGlobal :: JSGlobal
emptyGlobal = JSGlobal 1 Nothing Nothing Nothing Nothing Nothing


raiseError :: String -> Runtime a
raiseError s = throwError $ JSError (VStr s, [])

raiseProtoError :: ErrorType -> String -> Runtime a
raiseProtoError t msg = throwError $ JSProtoError (t, msg)

data ErrorType = ReferenceError | SyntaxError | TypeError deriving (Show, Eq)

data PrimitiveHint = HintNone | HintNumber | HintString deriving (Show, Eq)

debug :: Show a => a -> Runtime ()
debug a = do
  liftIO $ print a
