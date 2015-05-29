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
  let name = show errorType
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

dflt :: JSVal -> Maybe (PropDesc JSVal) -> Runtime JSVal
dflt d prop = case prop of
  Nothing -> return d
  Just desc -> propValue desc

errorToString :: JSFunction
errorToString (VObj this) _args = do
  name <- objGetProperty "name" this >>= dflt (VStr "Error")
  msg  <- objGetProperty "message" this >>= dflt VUndef
  return $ VStr $ showVal name ++ ": " ++ showVal msg
errorToString _ _ = raiseError "errorToString called on non-object"
