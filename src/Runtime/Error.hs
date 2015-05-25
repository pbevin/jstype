module Runtime.Error where

import Control.Monad.Except
import Control.Applicative
import Data.Maybe
import Runtime.Types
import Runtime.Object
import Runtime.Conversion




raise :: JSVal -> Runtime a
raise err = throwError (err, [])

raiseReferenceError :: String -> Runtime a
raiseReferenceError msg = createReferenceError (VStr msg) >>= raise

createReferenceError :: JSVal -> Runtime JSVal
createReferenceError = createError "ReferenceError"

raiseSyntaxError :: String -> Runtime a
raiseSyntaxError msg = createSyntaxError (VStr msg) >>= raise

createSyntaxError :: JSVal -> Runtime JSVal
createSyntaxError = createError "SyntaxError"

createError :: String -> JSVal -> Runtime JSVal
-- createError = do
--   prototype <- getGlobalProperty name
--   newObjectFromConstructor
createError name message = do
  prototype <- getGlobalProperty name
           >>= valGetProperty "prototype"
           >>= dflt VUndef

  obj <- newObject >>= addOwnProperty "name" (VStr name)
                   >>= addOwnProperty "prototype" prototype
  errConstructor (VObj obj) [message]

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

dflt :: JSVal -> Maybe JSVal -> Runtime JSVal
dflt d = return . fromMaybe d

errorToString :: JSFunction
errorToString (VObj this) _args = do
  obj  <- deref this
  name <- objGetProperty "name" obj >>= dflt (VStr "Error")
  msg  <- objGetProperty "message" obj >>= dflt VUndef
  return $ VStr $ showVal name ++ ": " ++ showVal msg
errorToString _ _ = raiseError "errorToString called on non-object"
