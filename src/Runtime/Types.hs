{-# LANGUAGE GeneralizedNewtypeDeriving, LambdaCase #-}

module Runtime.Types where

import Control.Monad.State
import Control.Monad.Except
import Control.Monad.Writer
import Control.Applicative
import Text.Show.Functions
import qualified Data.Map as M
import Data.Maybe
import Data.IORef
import Runtime.PropMap
import Expr
import JSNum

data JSVal = VNum JSNum
           | VStr String
           | VBool Bool
           | VRef JSRef
           | VUndef
           | VNull
           | VObj (Shared JSObj)
           | VNative String Int (JSVal -> [JSVal] -> Runtime JSVal)
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
  show (VNative n _ _)  = "VNative " ++ n
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
  getMethod :: Maybe (String -> Shared JSObj -> Runtime JSVal),
  getOwnPropertyMethod :: Maybe (String -> Shared JSObj -> Runtime (Maybe (PropDesc JSVal))),
  hasInstanceMethod :: Maybe (Shared JSObj -> JSVal -> Runtime Bool),
  defineOwnPropertyMethod :: Maybe (String -> PropDesc JSVal -> Bool -> Shared JSObj -> Runtime Bool),
  objPrimitiveValue :: Maybe JSVal,
  objParameterMap :: Maybe (Shared JSObj),
  objScope :: Maybe (Shared LexEnv),
  objFormalParameters :: Maybe ([Ident]),
  objCode :: Maybe Program,
  objExtensible :: Bool
} deriving Show

emptyObject :: JSObj
emptyObject = JSObj {
  objClass = "Object",
  ownProperties = emptyPropMap,
  objPrototype = Nothing,
  callMethod = Nothing,
  cstrMethod = Nothing,
  getMethod = Nothing,
  getOwnPropertyMethod = Nothing,
  hasInstanceMethod = Nothing,
  defineOwnPropertyMethod = Nothing,
  objPrimitiveValue = Nothing,
  objParameterMap = Nothing,
  objScope = Nothing,
  objFormalParameters = Nothing,
  objCode = Nothing,
  objExtensible = True
}

defineOwnProperty :: String -> PropDesc JSVal -> Bool -> Shared JSObj -> Runtime Bool
defineOwnProperty name desc strict objRef = (defineOwnPropertyMethod <$> deref objRef) >>= \case
  Nothing -> raiseProtoError ReferenceError "No defineOwnProperty method"
  Just m -> m name desc strict objRef

objGet :: String -> Shared JSObj -> Runtime JSVal
objGet p obj = do
  getMethod <$> deref obj >>= \case
    Nothing -> raiseError $ "No get method for " ++ show obj
    Just f -> f p obj

objGetOwnProperty :: String -> Shared JSObj -> Runtime (Maybe (PropDesc JSVal))
objGetOwnProperty p obj = do
  getOwnPropertyMethod <$> deref obj >>= \case
    Nothing -> raiseError $ "No getOwnProperty method for " ++ show obj
    Just f -> f p obj

isCallable :: JSVal -> Runtime (Maybe JSFunction)
isCallable (VObj obj) = callMethod <$> deref obj
isCallable _ = return Nothing



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
  VNative _ _ _ -> TypeFunction
  VObj _        -> TypeObject
  VStacktrace _ -> TypeOther "Stacktrace"
  _             -> error $ "No idea what type " ++ show v ++ " is..."

instance Eq JSVal where
  VNum a == VNum b               = a == b
  VStr a == VStr b               = a == b
  VObj a == VObj b               = a == b
  VBool a == VBool b             = a == b
  VNative a _ _ == VNative b _ _ = a == b -- XXX
  _a == _b = False


-- 9.12
sameValue :: JSVal -> JSVal -> Bool
sameValue (VNum (JSNum x)) (VNum (JSNum y))
  | isNaN x && isNaN y = True
  | otherwise          =  x == y
sameValue (VObj x) (VObj y) = x == y
sameValue x y = x == y


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

type StmtReturn = Result JSVal
data Result a = CTNormal   { rval :: Maybe a }
              | CTBreak    { rval :: Maybe a, label :: Maybe Ident }
              | CTContinue { rval :: Maybe a, label :: Maybe Ident }
              | CTReturn   { rval :: Maybe a }
              | CTThrow    { rval :: Maybe a }
              deriving (Show, Eq)


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
            MonadFix,
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
  globalEnvironment :: Maybe (Shared LexEnv),
  globalContext   :: Maybe JSCxt
}

emptyGlobal :: JSGlobal
emptyGlobal = JSGlobal 1 Nothing Nothing Nothing Nothing Nothing Nothing


raiseError :: String -> Runtime a
raiseError s = throwError $ JSError (VStr s, [])

raiseProtoError :: ErrorType -> String -> Runtime a
raiseProtoError t msg = throwError $ JSProtoError (t, msg)

data ErrorType = ReferenceError | SyntaxError | TypeError deriving (Show, Eq)

data PrimitiveHint = HintNone | HintNumber | HintString deriving (Show, Eq)

debug :: Show a => a -> Runtime a
debug a = do
  liftIO $ print a
  return a

finally :: Runtime a -> Runtime () -> Runtime a
finally a f = do
  (a <* f) `catchError` (\e -> f >> throwError e)

type PropertyMap = PropMap Ident (PropDesc JSVal)

data Property a = PropValue a
                | PropFlag Bool
                | PropGetter (JSVal -> Runtime a)
                | PropSetter (JSVal -> a -> Runtime ())
                deriving Show

newtype PropDesc a = PropDesc (M.Map String (Property a)) deriving Show

instance Monoid (PropDesc a) where
  mempty = PropDesc (M.empty)
  mappend (PropDesc m1) (PropDesc m2) = PropDesc (M.union m2 m1)
