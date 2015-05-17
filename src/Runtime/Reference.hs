module Runtime.Reference where

import Control.Monad.Except
import qualified Data.Map as M
import Data.Maybe
import Runtime.Object
import Runtime.Types

-- ref 8.7.1
getValue :: JSVal -> JSRuntime JSVal
getValue v
  | typeof v /= TypeReference   = return v
  | isUnresolvableReference ref = raiseError $ "ReferenceError: " ++ show ref
  | isPropertyReference ref     = getValuePropertyReference ref
  | otherwise                   = getValueEnvironmentRecord ref
    where ref = unwrapRef v

-- ref 8.7.2
putValue :: JSVal -> JSVal -> JSRuntime ()
putValue v w
  | typeof v /= TypeReference   = raiseError $ "ReferenceError: " ++ show v
  | isUnresolvableReference ref = putUnresolvable ref w
  | isPropertyReference ref     = putPropertyReference ref w
  | otherwise                   = putEnvironmentRecord ref w
    where ref = unwrapRef v

-- ref 8.7

hasPrimitiveBase :: JSRef -> Bool
hasPrimitiveBase ref =
  case getBase ref of
    VBool _ -> True
    VStr  _ -> True
    VNum  _ -> True
    _       -> False


isPropertyReference :: JSRef -> Bool
isPropertyReference ref =
  case getBase ref of
    VObj _ -> True
    _      -> hasPrimitiveBase ref

isUnresolvableReference :: JSRef -> Bool
isUnresolvableReference ref =
  case getBase ref of
    VUndef -> True
    _      -> False



getValuePropertyReference :: JSRef -> JSRuntime JSVal
getValuePropertyReference (JSRef (VObj objref) name _isStrict) = do
  obj <- deref objref
  val <- objGetProperty name obj
  case val of
    Nothing -> return VUndef
    Just v  -> return v

getValuePropertyReference _ = error "Internal error in getValuePropertyReference"



-- ref 10.2.1.1.4 incomplete
getValueEnvironmentRecord :: JSRef -> JSRuntime JSVal
getValueEnvironmentRecord (JSRef (VCxt (JSCxt envref _ _)) name _isStrict) = do
  env <- deref envref
  return $ fromMaybe VUndef $ M.lookup name env
getValueEnvironmentRecord x = raiseError $ "Internal error in getValueEnvironmentRecord: " ++ show x



putUnresolvable :: JSRef -> JSVal -> JSRuntime ()
putUnresolvable _ref _val = error "Can't putUnresolvable"

putPropertyReference :: JSRef -> JSVal -> JSRuntime ()
putPropertyReference (JSRef (VObj objref) name _isStrict) val = modifyRef objref setRef where
    setRef obj = obj { ownProperties = M.insert name val (ownProperties obj) }
putPropertyReference _ _ = error "Internal error in putPropertyReference"

putEnvironmentRecord :: JSRef -> JSVal -> JSRuntime ()
putEnvironmentRecord (JSRef (VCxt (JSCxt envref _ _)) name _isStrict) val = modifyRef envref setRef where
  setRef = M.insert name val
putEnvironmentRecord _ _ = error "Internal error in putEnvironmentRecord"



unwrapRef :: JSVal -> JSRef
unwrapRef (VRef ref) = ref
unwrapRef v = error $ "Can't unwrap " ++ show v
