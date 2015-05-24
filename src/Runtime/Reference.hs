module Runtime.Reference where

import Control.Monad.Except
import qualified Data.Map as M
import Data.Maybe
import Runtime.Object
import Runtime.Types
import Runtime.Global (getGlobalObject)
import Runtime.Error
import Expr

import Debug.Trace

-- ref 8.7.1
getValue :: JSVal -> Runtime JSVal
getValue v
  | typeof v /= TypeReference   = return v
  | isUnresolvableReference ref = raiseError $ "ReferenceError: No such variable " ++ getReferencedName ref
  | isPropertyReference ref     = getValuePropertyReference ref
  | otherwise                   = getValueEnvironmentRecord ref
    where ref = unwrapRef v

-- ref 8.7.2
putValue :: JSRef -> JSVal -> Runtime ()
putValue ref w
  | isUnresolvableReference ref = putUnresolvable ref w
  | isPropertyReference ref     = putPropertyReference ref w
  | otherwise                   = putEnvironmentRecord ref w

-- ref 8.7
hasPrimitiveBase :: JSRef -> Bool
hasPrimitiveBase ref =
  case getBase ref of
    VBool _ -> True
    VStr  _ -> True
    VNum  _ -> True
    _       -> False


-- ref 8.7
isPropertyReference :: JSRef -> Bool
isPropertyReference ref =
  case getBase ref of
    VObj _ -> True
    _      -> hasPrimitiveBase ref

-- ref 8.7
isUnresolvableReference :: JSRef -> Bool
isUnresolvableReference ref =
  case getBase ref of
    VUndef -> True
    _      -> False


getValuePropertyReference :: JSRef -> Runtime JSVal
getValuePropertyReference (JSRef (VObj objref) name _isStrict) = do
  obj <- deref objref
  val <- objGetProperty name obj
  case val of
    Nothing -> return VUndef
    Just v  -> return v

getValuePropertyReference _ = error "Internal error in getValuePropertyReference"



-- ref 10.2.1.1.4 incomplete
getValueEnvironmentRecord :: JSRef -> Runtime JSVal
getValueEnvironmentRecord (JSRef (VEnv envRef) name _isStrict) = do
  val <- deref envRef >>= envLookup name . envRec
  return $ fromMaybe VUndef $ val
getValueEnvironmentRecord x = raiseError $ "Internal error in getValueEnvironmentRecord: " ++ show x



putUnresolvable :: JSRef -> JSVal -> Runtime ()
putUnresolvable ref val =
  if isStrictReference ref
  then raiseReferenceError $ getReferencedName ref ++ " is not defined"
  else void $ getGlobalObject >>= addOwnProperty (getReferencedName ref) val

isStrictReference :: JSRef -> Bool
isStrictReference ref = strictness ref == Strict


putPropertyReference :: JSRef -> JSVal -> Runtime ()
putPropertyReference (JSRef (VObj objref) name _isStrict) val = modifyRef objref setRef where
    setRef obj = obj { ownProperties = M.insert name val (ownProperties obj) }
putPropertyReference _ _ = error "Internal error in putPropertyReference"

putEnvironmentRecord :: JSRef -> JSVal -> Runtime ()
putEnvironmentRecord (JSRef (VEnv envRef) name _isStrict) val = do
  lx <- deref envRef
  envInsert name val (envRec lx)
putEnvironmentRecord _ _ = error "Internal error in putEnvironmentRecord"



unwrapRef :: JSVal -> JSRef
unwrapRef (VRef ref) = ref
unwrapRef v = error $ "Can't unwrap " ++ show v

isReference :: JSVal -> Bool
isReference (VRef _) = True
isReference _ = False


hasBinding :: Ident -> EnvRec -> Runtime Bool
hasBinding name (DeclEnvRec m) = liftM (M.member name) $ deref m
hasBinding name (ObjEnvRec obj) = liftM (M.member name . ownProperties) $ deref obj

envLookup :: Ident -> EnvRec -> Runtime (Maybe JSVal)
envLookup name (DeclEnvRec m) = liftM (M.lookup name) $ deref m
envLookup name (ObjEnvRec obj) = liftM (M.lookup name . ownProperties) $ deref obj

envInsert :: Ident -> JSVal -> EnvRec -> Runtime ()
envInsert name val (DeclEnvRec m) = modifyRef m (M.insert name val)
envInsert name val (ObjEnvRec obj) = void $ addOwnProperty name val obj
