module Runtime.Error where

import Control.Monad.Except
import Control.Applicative
import Data.Maybe
import Runtime.Types
import Runtime.Object
import Runtime.Conversion




raise :: JSVal -> Runtime a
raise err = throwError $ JSError (err, [])

raiseReferenceError :: String -> Runtime a
raiseReferenceError msg = createError ReferenceError (VStr msg) >>= raise

raiseSyntaxError :: String -> Runtime a
raiseSyntaxError msg = createError SyntaxError (VStr msg) >>= raise

raiseTypeError :: String -> Runtime a
raiseTypeError msg = createError TypeError (VStr msg) >>= raise

createError :: ErrorType -> JSVal -> Runtime JSVal
createError errorType message = do
  prototype <- objFindPrototype name
  obj <- newObject >>= addOwnProperty "name" (VStr name)
                   >>= objSetPrototype prototype
  errConstructor (VObj obj) [message]
  where name = show errorType

errFunction :: JSVal -> [JSVal] -> Runtime JSVal
errFunction this args = do
  className <- valClassName this
  obj <- newObject >>= setClass className
  errConstructor (VObj obj) args

errConstructor :: JSVal -> [JSVal] -> Runtime JSVal
errConstructor this args =
  let text = head args
  in case this of
    VObj obj -> do
      name <- objClassName obj
      liftM VObj $ setClass name obj
               >>= addOwnProperty "message" text
               >>= addOwnProperty "toString" (VNative errorToString)
    _ -> raiseError "Bad this for Error constructor"

dflt :: JSVal -> JSVal -> JSVal
dflt d prop = case prop of
  VUndef -> d
  _      -> prop

errorToString :: JSFunction
errorToString (VObj this) _args = do
  name <- dflt (VStr "Error") <$> objGet "name" this
  msg  <- objGet "message" this
  return $ VStr $ showVal name ++ ": " ++ showVal msg
errorToString _ _ = raiseError "errorToString called on non-object"
