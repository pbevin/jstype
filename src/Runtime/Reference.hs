module Runtime.Reference where

import Control.Monad.Except
import Data.Maybe
import Runtime.Object
import Runtime.Types
import Runtime.Global (getGlobalObject)
import Runtime.Error
import Runtime.Conversion
import Runtime.PropMap
import Expr

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


-- ref 8.7.1
getValuePropertyReference :: JSRef -> Runtime JSVal
getValuePropertyReference (JSRef base name _isStrict) =
  case base of
    VObj objRef -> getValueObject name objRef
    _           -> getValuePrimitive name base

-- ref 8.7.1
getValueObject :: String -> Shared JSObj -> Runtime JSVal
getValueObject name objref = do
  val <- objGetProperty name objref
  case val of
    Nothing -> return VUndef
    Just v  -> propValue v (VObj objref)

-- ref 8.7.1
getValuePrimitive :: String -> JSVal -> Runtime JSVal
getValuePrimitive name base = do
  o <- toObject base
  desc <- objGetProperty name o
  case desc of
    Nothing -> return VUndef
    Just (DataPD v _ _ _) -> return v
    Just (AccessorPD Nothing _ _ _) -> return VUndef
    Just (AccessorPD (Just getter) _ _ _) -> getter base




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


-- ref 10.2.1.1.1
hasBinding :: Ident -> EnvRec -> Runtime Bool
hasBinding name (DeclEnvRec m) = liftM (propMapMember name) $ deref m
hasBinding name (ObjEnvRec obj _) = liftM (propMapMember name . ownProperties) $ deref obj

-- ref 10.2.1.1.2, incomplete (D parameter)
createMutableBinding :: Ident -> JSEnv -> Runtime ()
createMutableBinding n envRef = do
  lx <- deref envRef
  envInsert n VUndef (envRec lx)

-- ref 10.2.1.1.3, incomplete (S parameter)
setMutableBinding :: Ident -> JSVal -> Bool -> JSEnv -> Runtime ()
setMutableBinding n v _s envRef = do
  lx <- deref envRef
  -- XXX deletable bindings
  envInsert n v (envRec lx)

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
  -- XXX deletable bindings
  return (VBool False)



envLookup :: Ident -> EnvRec -> Runtime (Maybe JSVal)
envLookup name (DeclEnvRec m) = lk VUndef name =<< deref m -- XXX VUndef for this
envLookup name (ObjEnvRec obj _) = lk (VObj obj) name . ownProperties =<< deref obj

lk :: Ord k => JSVal -> k -> PropMap k (PropDesc a) -> Runtime (Maybe a)
lk this k m = case propMapLookup k m of
  Nothing -> return Nothing
  Just desc -> Just <$> propValue desc this

envInsert :: Ident -> JSVal -> EnvRec -> Runtime ()
envInsert name val (DeclEnvRec m) = modifyRef m (propMapInsert name (valueToProp val))
envInsert name val (ObjEnvRec obj _) = void $ addOwnProperty name val obj

envDelete :: Ident -> EnvRec -> Runtime JSVal
envDelete name (DeclEnvRec m) = modifyRef m (propMapDelete name) >> return (VBool True)
envDelete name (ObjEnvRec obj _) = objDelete name False obj

-- ref 8.10.5, incomplete
toPropertyDescriptor :: JSVal -> Runtime (PropDesc JSVal)
toPropertyDescriptor (VObj objRef) = do
  enum <- toBoolean <$> objGet "enumerable" objRef
  conf <- toBoolean <$> objGet "configurable" objRef
  value <- objGet "value" objRef
  writable <- toBoolean <$> objGet "writable" objRef
  get <- objGet "get" objRef >>= mkGetter
  set <- objGet "set" objRef >>= mkSetter

  if isJust get || isJust set
  then return $ AccessorPD get set enum conf
  else return $ DataPD value writable enum conf

toPropertyDescriptor other = raiseProtoError TypeError $ "Can't convert " ++ show other ++ " to type descritor"

mkGetter :: JSVal -> Runtime (Maybe (JSVal -> Runtime JSVal))
mkGetter (VObj obj) = do
  call <- callMethod <$> deref obj
  case call of
    Nothing -> return Nothing
    Just f -> return $ Just (\this -> f this [])
mkGetter _ = return Nothing


mkSetter :: JSVal -> Runtime (Maybe (JSVal -> Runtime ()))
mkSetter (VObj obj) = do
  call <- callMethod <$> deref obj
  case call of
    Just setter -> return $ Just (\a -> void $ setter VUndef [a])
    Nothing -> raiseProtoError TypeError $ "Setter not defined"
mkSetter _ = return Nothing
