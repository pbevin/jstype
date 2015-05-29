module Runtime.Reference where

import Control.Monad.Except
import Data.Maybe
import Runtime.Object
import Runtime.Types
import Runtime.Global (getGlobalObject)
import Runtime.Error
import Runtime.PropMap
import Expr

import Debug.Trace

-- ref 8.7.1
getValue :: JSVal -> Runtime JSVal
getValue v
  | typeof v /= TypeReference   = return v
  | isUnresolvableReference ref = raiseReferenceError $ "No such variable " ++ getReferencedName ref
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
  val <- objGetProperty name objref
  case val of
    Nothing -> return VUndef
    Just v  -> propValue v
getValuePropertyReference _ = error "Internal error in getValuePropertyReference"



-- ref 10.2.1.1.4 incomplete
getValueEnvironmentRecord :: JSRef -> Runtime JSVal
getValueEnvironmentRecord (JSRef (VEnv envRef) name _isStrict) = do
  val <- deref envRef >>= envLookup name . envRec
  return $ fromMaybe VUndef $ val
getValueEnvironmentRecord x = raiseError $ "Internal error in getValueEnvironmentRecord: " ++ show x

-- ref 10.2.1.2.5
deleteBinding :: String -> JSEnv -> Runtime JSVal
deleteBinding n envRef = do
  val <- deref envRef
  envDelete n (envRec val)

  




putUnresolvable :: JSRef -> JSVal -> Runtime ()
putUnresolvable ref val =
  if isStrictReference ref
  then raiseReferenceError $ getReferencedName ref ++ " is not defined"
  else void $ getGlobalObject >>= addOwnProperty (getReferencedName ref) val

isStrictReference :: JSRef -> Bool
isStrictReference ref = refStrictness ref == Strict


putPropertyReference :: JSRef -> JSVal -> Runtime ()
putPropertyReference (JSRef (VObj objRef) name strict) val =
  objPut name val (strict == Strict) objRef
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
hasBinding name (DeclEnvRec m) = liftM (propMapMember name) $ deref m
hasBinding name (ObjEnvRec obj) = liftM (propMapMember name . ownProperties) $ deref obj

envLookup :: Ident -> EnvRec -> Runtime (Maybe JSVal)
envLookup name (DeclEnvRec m) = lk name =<< deref m
envLookup name (ObjEnvRec obj) = lk name . ownProperties =<< deref obj

lk :: Ord k => k -> PropMap k (PropDesc a) -> Runtime (Maybe a)
lk k m = case propMapLookup k m of
  Nothing -> return Nothing
  Just desc -> Just <$> propValue desc

envInsert :: Ident -> JSVal -> EnvRec -> Runtime ()
envInsert name val (DeclEnvRec m) = modifyRef m (propMapInsert name (valueToProp val))
envInsert name val (ObjEnvRec obj) = void $ addOwnProperty name val obj

envDelete :: Ident -> EnvRec -> Runtime JSVal
envDelete name (DeclEnvRec m) = modifyRef m (propMapDelete name) >> return (VBool True)
envDelete name (ObjEnvRec obj) = objDelete name False obj
