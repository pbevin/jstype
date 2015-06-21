module Runtime.Reference where

import Control.Lens hiding (enum, strict)
import Control.Monad hiding (guard)
import Data.Maybe
import qualified Data.Map as M
import Runtime.Object
import Runtime.Types
import Runtime.Global (getGlobalObject)
import Runtime.Error
import Runtime.Conversion
import Runtime.PropMap
import Runtime.PropertyDescriptor
import Runtime.Shared
import Expr

-- ref 8.7.1
getValue :: JSVal -> Runtime JSVal
getValue v
  | typeof v /= TypeReference   = return v
  | isUnresolvableReference ref = raiseReferenceError $ "No such variable " ++ getReferencedName ref
  | isPropertyReference ref     = case base of
      VObj objRef -> objGetProperty name objRef >>= toValue
      _           -> toObject base >>= objGetProperty name >>= toValue
  | VEnv env <- base            = getBindingValue name strict env
  | otherwise                   = raiseReferenceError $ "Can't lookup " ++ show name ++ " in reference " ++ show ref
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
          putUnresolvable :: JSRef -> JSVal -> Runtime ()
          putUnresolvable ref val =
            if isStrictReference ref
            then raiseReferenceError $ getReferencedName ref ++ " is not defined"
            else void $ getGlobalObject >>= objPut (getReferencedName ref) val (isStrictReference ref)

          guard :: Bool -> Runtime ()
          guard cond = unless (cond) $ do
            if strict == Strict
            then raiseTypeError $ "Cannot set " ++ name
            else exit

          putPropertyReference :: JSRef -> JSVal -> Runtime ()
          putPropertyReference (JSRef (VObj objRef) refName refStrict) val =
            objPut refName val (refStrict == Strict) objRef

          putPropertyReference _other value = withGuard $ do
            o <- toObject base
            canPut <- objCanPut name o
            guard canPut

            ownDesc <- objGetOwnProperty name o
            guard (not $ isDataDescriptor ownDesc)

            desc <- objGetProperty name o
            guard (isAccessorDescriptor desc)

            let Just d = desc
                Just s = propSetter d
            s base value


putValue' :: JSVal -> JSVal -> Runtime ()
putValue' (VRef ref) = putValue ref
putValue' _ = const $ raiseTypeError "Not assignable"

-- ref 8.7
hasPrimitiveBase :: JSRef -> Bool
hasPrimitiveBase ref =
  case getBase ref of
    VBool _   -> True
    VStr  _   -> True
    VNum  _   -> True
    VNative{} -> True
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


isStrictReference :: JSRef -> Bool
isStrictReference ref = refStrictness ref == Strict



unwrapRef :: JSVal -> JSRef
unwrapRef (VRef ref) = ref
unwrapRef v = error $ "Can't unwrap " ++ show v

isReference :: JSVal -> Bool
isReference (VRef _) = True
isReference _ = False


-- ref 10.2.1.1.1
-- ref 10.2.1.2.1
hasBinding :: Ident -> EnvRec -> Runtime Bool
hasBinding name (DeclEnvRec m) = liftM (propMapMember name) $ deref m
hasBinding name (ObjEnvRec obj _) = liftM (propMapMember name . view ownProperties) $ deref obj

-- ref 10.2.1.1.2
-- ref 10.2.1.2.2
createMutableBinding :: Ident -> Bool -> EnvRec -> Runtime ()
createMutableBinding n d (DeclEnvRec m ) = modifyRef m (propMapInsert' n (dataPD VUndef True True True) d)
createMutableBinding n d (ObjEnvRec obj _) = void $ addOwnPropertyDescriptor n (dataPD VUndef True True d) obj

-- ref 10.2.1.1.3
-- ref 10.2.1.2.3
setMutableBinding :: Ident -> JSVal -> Bool -> EnvRec -> Runtime ()
setMutableBinding name val d (DeclEnvRec m) = modifyRef m (propMapInsert' name (dataPD val True True True) d)
setMutableBinding name val d (ObjEnvRec obj _) = void $ objPut name val d obj

-- ref 10.2.1.1.4
-- ref 10.2.1.2.4
getBindingValue :: Ident -> Strictness -> EnvRec -> Runtime JSVal
getBindingValue n s env = case env of
  DeclEnvRec m -> lk VUndef n =<< deref m -- XXX VUndef for this
  ObjEnvRec obj _ -> lk (VObj obj) n . view ownProperties =<< deref obj
  where
    lk this k m = case propMapLookup k m of
      Nothing   -> return VUndef
      Just desc -> getValueFrom this desc


-- -- XXX deprecate this!
-- getValueEnvironmentRecord :: JSVal -> Ident -> Runtime JSVal
-- getValueEnvironmentRecord (VEnv env) name = getBindingValue name NotStrict env

-- ref 10.2.1.1.5
-- ref 10.2.1.2.5
deleteBinding :: String -> EnvRec -> Runtime JSVal
deleteBinding _ (DeclEnvRec _) = return (VBool False)
deleteBinding n (ObjEnvRec obj _) = objDelete n False obj

-- ref 8.10.5, incomplete
toPropertyDescriptor :: JSVal -> Runtime (PropDesc JSVal)
toPropertyDescriptor (VObj objRef) = do
  enum <- toBoolean <$> objGet "enumerable" objRef
  conf <- toBoolean <$> objGet "configurable" objRef
  value <- objGet "value" objRef
  writable <- toBoolean <$> objGet "writable" objRef
  getter <- objGet "get" objRef >>= mkGetter
  setter <- objGet "set" objRef >>= mkSetter

  if isJust getter || isJust setter
  then return $ accessorPD getter setter enum conf
  else return $ dataPD value writable enum conf

toPropertyDescriptor other = raiseProtoError TypeError $ "Can't convert " ++ show other ++ " to type descritor"

mkGetter :: JSVal -> Runtime (Maybe (JSVal -> Runtime JSVal))
mkGetter (VObj obj) = do
  call <- (^.callMethod) <$> deref obj
  case call of
    Nothing -> return Nothing
    Just f -> return $ Just (\this -> f this [])
mkGetter _ = return Nothing


mkSetter :: JSVal -> Runtime (Maybe (JSVal -> JSVal -> Runtime ()))
mkSetter (VObj obj) = do
  call <- (^.callMethod) <$> deref obj
  case call of
    Just setter -> return $ Just (\this val -> void $ setter this [val])
    Nothing -> raiseProtoError TypeError $ "Setter not defined"
mkSetter _ = return Nothing

getValueFrom :: JSVal -> PropDesc JSVal -> Runtime JSVal
getValueFrom base d
  | isDataDescriptor (Just d) = return $ fromMaybe VUndef (propValue d)
  | isAccessorDescriptor (Just d) =
    case propGetter d of
      Nothing -> return VUndef
      Just get -> get base
  | otherwise = return VUndef
