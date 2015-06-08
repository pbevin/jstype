module Runtime.Reference where

import Control.Monad.Except
import Data.Maybe
import qualified Data.Map as M
import Runtime.Object
import Runtime.Types
import Runtime.Global (getGlobalObject)
import Runtime.Error
import Runtime.Conversion
import Runtime.PropMap
import Runtime.PropertyDescriptor
import Expr

-- ref 8.7.1
getValue :: JSVal -> Runtime JSVal
getValue v
  | typeof v /= TypeReference   = return v
  | isUnresolvableReference ref = raiseReferenceError $ "No such variable " ++ getReferencedName ref
  | isPropertyReference ref     = case base of
      VObj objRef -> objGetProperty name objRef >>= toValue
      _           -> toObject base >>= objGetProperty name >>= toValue
  | otherwise                   = getValueEnvironmentRecord base name
    where ref@(JSRef base name strict) = unwrapRef v
          toValue desc = case desc of
              Nothing -> return VUndef
              Just pd -> getValueFrom base pd

-- ref 8.7.2
putValue :: JSRef -> JSVal -> Runtime ()
putValue ref@(JSRef base name strict) w
  | isUnresolvableReference ref = putUnresolvable ref w
  | isPropertyReference ref     = putPropertyReference ref w
  | otherwise                   = setMutableBinding name w (strict == Strict) env
    where (VEnv env) = base

-- ref 8.7
hasPrimitiveBase :: JSRef -> Bool
hasPrimitiveBase ref =
  case getBase ref of
    VBool _   -> True
    VStr  _   -> True
    VNum  _   -> True
    VNative _ -> True
    _         -> False


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

-- ref 10.2.1.1.2
createMutableBinding :: Ident -> Bool -> JSEnv -> Runtime ()
createMutableBinding n d envRef = do
  lx <- deref envRef
  envInsert n VUndef d (envRec lx)

-- ref 10.2.1.1.3
setMutableBinding :: Ident -> JSVal -> Bool -> EnvRec -> Runtime ()
setMutableBinding = envInsert


-- ref 10.2.1.1.4
getValueEnvironmentRecord :: JSVal -> Ident -> Runtime JSVal
getValueEnvironmentRecord (VEnv env) name = case env of
  DeclEnvRec m -> lk VUndef name =<< deref m -- XXX VUndef for this
  ObjEnvRec obj _ -> lk (VObj obj) name . ownProperties =<< deref obj
  where
    lk this k m = case propMapLookup k m of
      Nothing   -> return VUndef
      Just desc -> getValueFrom this desc

deleteBinding :: String -> EnvRec -> Runtime JSVal
-- ref 10.2.1.1.5 (DeclEnvRec)
deleteBinding _ (DeclEnvRec _) = return (VBool False)
-- ref 10.2.1.2.5 (ObjEnvRec)
deleteBinding n (ObjEnvRec obj _) = objDelete n False obj


envInsert :: Ident -> JSVal -> Bool -> EnvRec -> Runtime ()
envInsert name val d (DeclEnvRec m) = do
  modifyRef m (propMapInsert' name (dataPD val True True True) d)
envInsert name val d (ObjEnvRec obj _) = void $ do
  addOwnPropertyDescriptor name desc obj
    where desc = dataPD val True True d

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
  then return $ accessorPD get set enum conf
  else return $ dataPD value writable enum conf

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

getValueFrom :: JSVal -> PropDesc JSVal -> Runtime JSVal
getValueFrom base d
  | isDataDescriptor (Just d) = return $ fromMaybe VUndef (propValue d)
  | isAccessorDescriptor (Just d) =
    case propGetter d of
      Nothing -> return VUndef
      Just g -> g base
  | otherwise = return VUndef
