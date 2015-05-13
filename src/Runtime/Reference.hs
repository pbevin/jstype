module Runtime.Reference where

import Control.Monad.Except
import qualified Data.Map as M
import Data.IORef
import Runtime.Object
import Runtime.Types

-- ref 8.7.1
getValue :: JSVal -> JSRuntime JSVal
getValue v
  | typeof v /= TypeReference   = return v
  | isUnresolvableReference ref = throwError "ReferenceError"
  | isPropertyReference ref     = getValuePropertyReference ref
  | otherwise                   = getValueEnvironmentRecord ref
    where ref = unwrapRef v

-- ref 8.7.2
putValue :: JSVal -> JSVal -> JSRuntime ()
putValue v w
  | typeof v /= TypeReference   = throwError "ReferenceError"
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
getValuePropertyReference (JSRef (VObj objref) name isStrict) = do
  obj <- liftIO $ readIORef objref
  val <- objGetProperty obj name
  case val of
    Nothing -> return VUndef
    Just v  -> return v

getValuePropertyReference _ = error "Internal error in getValuePropertyReference"



-- ref 10.2.1.1.4 incomplete
getValueEnvironmentRecord :: JSRef -> JSRuntime JSVal
getValueEnvironmentRecord (JSRef (VEnv envref) name isStrict) = liftIO $ do
  env <- readIORef envref
  maybe (return VUndef) readIORef $ M.lookup name env
getValueEnvironmentRecord _ = error "Internal error in getValueEnvironmentRecord"







--   | hasPrimitiveBase v        -> undefined -- 4b
--   | isPropertyReference v     -> undefined -- 4a
--   | otherwise                 -> get



--   | otherwise                 -> getValue' $ unwrapRef v

-- getValue' :: JSRef -> JSRuntime JSVal
-- getValue' ref = do
--   base <- getBase ref

--   case v of
--     VRef ref -> readIORef ref
--     other -> return other



putUnresolvable :: JSRef -> JSVal -> JSRuntime ()
putUnresolvable _ref _val = error "Can't putUnresolvable"

putPropertyReference :: JSRef -> JSVal -> JSRuntime ()
putPropertyReference (JSRef (VObj objref) name isStrict) val = liftIO $ do
  modifyIORef objref setRef where
    setRef obj = obj { ownProperties = M.insert name val (ownProperties obj) }
putPropertyReference _ _ = error "Internal error in putPropertyReference"

putEnvironmentRecord :: JSRef -> JSVal -> JSRuntime ()
putEnvironmentRecord (JSRef (VEnv envref) name isStrict) val = liftIO $ do
  env <- readIORef envref
  case M.lookup name env of
    Just ref -> writeIORef ref val
    Nothing -> do
      ref <- newIORef val
      writeIORef envref (M.insert name ref env)
putEnvironmentRecord _ _ = error "Internal error in putEnvironmentRecord"



unwrapRef :: JSVal -> JSRef
unwrapRef (VRef ref) = ref
unwrapRef v = error $ "Can't unwrap " ++ show v
