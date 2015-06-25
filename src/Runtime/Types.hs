{-# LANGUAGE GeneralizedNewtypeDeriving, LambdaCase, TemplateHaskell, RankNTypes #-}

module Runtime.Types (module Runtime.Types, liftIO) where

import Control.Lens
import Control.Monad.RWS.Strict
import Control.Monad.Except
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
           | VRegExp String String

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
  show (VRegExp p f)    = "VRegExp " ++ show p ++ " " ++ show f

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
  _objClass :: String,
  _ownProperties :: PropertyMap,
  _objPrototype :: Maybe (Shared JSObj),
  _callMethod :: Maybe JSFunction,
  _cstrMethod :: Maybe JSFunction,
  _getMethod :: Maybe (String -> Shared JSObj -> Runtime JSVal),
  _getOwnPropertyMethod :: Maybe (String -> Shared JSObj -> Runtime (Maybe (PropDesc JSVal))),
  _hasInstanceMethod :: Maybe (Shared JSObj -> JSVal -> Runtime Bool),
  _defineOwnPropertyMethod :: Maybe (String -> PropDesc JSVal -> Bool -> Shared JSObj -> Runtime Bool),
  _objPrimitiveValue :: Maybe JSVal,
  _objParameterMap :: Maybe (Shared JSObj),
  _objScope :: Maybe (Shared LexEnv),
  _objFormalParameters :: Maybe ([Ident]),
  _objCode :: Maybe Program,
  _objExtensible :: Bool
} deriving Show


emptyObject :: JSObj
emptyObject = JSObj {
  _objClass = "Object",
  _ownProperties = emptyPropMap,
  _objPrototype = Nothing,
  _callMethod = Nothing,
  _cstrMethod = Nothing,
  _getMethod = Nothing,
  _getOwnPropertyMethod = Nothing,
  _hasInstanceMethod = Nothing,
  _defineOwnPropertyMethod = Nothing,
  _objPrimitiveValue = Nothing,
  _objParameterMap = Nothing,
  _objScope = Nothing,
  _objFormalParameters = Nothing,
  _objCode = Nothing,
  _objExtensible = True
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
  VUndef == VUndef               = True
  VNull == VNull                 = True
  VRegExp a b == VRegExp a' b'   = a == a' && b == b'
  _a == _b = False


-- 9.12
sameValue :: JSVal -> JSVal -> Bool
sameValue (VNum x) (VNum y)
  | isNaN x && isNaN y = True
  | otherwise          =  x == y
sameValue (VObj x) (VObj y) = x == y
sameValue x y = x == y


type JSOutput = String
data JSError = JSError (JSVal, [SrcLoc])
             | JSProtoError (ErrorType, String)
             | EarlyExit
             deriving (Show, Eq)
type JSFunction = JSVal -> [JSVal] -> Runtime JSVal

type StmtReturn = Result JSVal
data Result a = CTNormal   { rval :: Maybe a }
              | CTBreak    { rval :: Maybe a, label :: Maybe Ident }
              | CTContinue { rval :: Maybe a, label :: Maybe Ident }
              | CTReturn   { rval :: Maybe a }
              | CTThrow    { rval :: Maybe a }
              deriving (Show, Eq)


-- Shared type
type ObjId = Int
data Shared a = Shared {
  _where :: Simple Lens Store (Maybe a),
  objid :: ObjId
}

instance Show (Shared a) where
  show a = "(shared #" ++ show (objid a) ++ ")"

instance Eq (Shared a) where
  a == b = objid a == objid b

newtype Runtime a = JS {
  unJS :: ExceptT JSError (RWST JSGlobal String Store IO) a
} deriving (Monad, MonadIO,
            MonadReader JSGlobal,
            MonadWriter String,
            MonadState Store,
            MonadRWS JSGlobal String Store,
            MonadError JSError,
            MonadFix,
            Functor,
            Applicative)

instance Alternative Runtime where
  (<|>) = mplus
  empty = mzero

instance MonadPlus Runtime where
  mzero = throwError EarlyExit

runRuntime :: JSGlobal -> Store -> Runtime a -> IO (Either JSError a, Store, String)
runRuntime r s a = runRWST (runExceptT $ unJS a) r s

withGuard :: Runtime () -> Runtime ()
withGuard p = p `catchError` (\e -> if e == EarlyExit then return () else throwError e)

exit :: Runtime a
exit = throwError EarlyExit

data JSGlobal = JSGlobal {
  globalObject          :: Shared JSObj,
  globalObjectPrototype :: Shared JSObj,
  globalEvaluator       :: EvalCallType -> String -> Runtime StmtReturn,
  globalRun             :: [Statement] -> Runtime StmtReturn,
  globalEnvironment     :: Shared LexEnv,
  globalContext         :: JSCxt
}

data Store = Store {
  storeNextID           :: Int,
  _storeObjStore  :: M.Map ObjId JSObj,
  _storePropMapStore  :: M.Map ObjId PropertyMap,
  _storeLexEnvStore  :: M.Map ObjId LexEnv
}

raiseError :: String -> Runtime a
raiseError s = throwError $ JSError (VStr s, [])

raiseProtoError :: ErrorType -> String -> Runtime a
raiseProtoError t msg = throwError $ JSProtoError (t, msg)

data ErrorType = ReferenceError | SyntaxError | TypeError deriving (Show, Eq)
data PrimitiveHint = HintNone | HintNumber | HintString deriving (Show, Eq)
data EvalCallType = DirectEvalCall | IndirectEvalCall deriving (Show, Eq)

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


makeLenses ''JSObj
makeLenses ''Store
