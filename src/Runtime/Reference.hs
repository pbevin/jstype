module Runtime.Reference where

import Control.Monad.Except
import Data.IORef
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
getValuePropertyReference ref = return $ VStr "qqq"
getValueEnvironmentRecord :: JSRef -> JSRuntime JSVal
getValueEnvironmentRecord ref = return $ VStr "www"




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
putUnresolvable ref val = return ()

putPropertyReference :: JSRef -> JSVal -> JSRuntime ()
putPropertyReference ref val = return ()

putEnvironmentRecord :: JSRef -> JSVal -> JSRuntime ()
putEnvironmentRecord ref val = return ()










unwrapRef :: JSVal -> JSRef
unwrapRef (VRef ref) = ref
unwrapRef v = error $ "Can't unwrap " ++ show v
