{-# LANGUAGE OverloadedStrings #-}

module Runtime.Error where

import Control.Monad.Except
import Control.Applicative
import Data.Maybe
import Data.Monoid
import qualified Data.Text as T
import Data.Text (Text)
import Runtime.Types
import Runtime.Object
import Runtime.Conversion




raise :: JSVal -> Runtime a
raise err = throwError $ JSError (err, [])

raiseReferenceError :: Text -> Runtime a
raiseReferenceError msg = createError ReferenceError (VStr msg) >>= raise

raiseSyntaxError :: Text -> Runtime a
raiseSyntaxError msg = createError SyntaxError (VStr msg) >>= raise

raiseTypeError :: Text -> Runtime a
raiseTypeError msg = createError TypeError (VStr msg) >>= raise

raiseURIError :: Text -> Runtime a
raiseURIError msg = createError URIError (VStr msg) >>= raise

createError :: ErrorType -> JSVal -> Runtime JSVal
createError errorType message = do
  prototype <- objFindPrototype name
  cstr <- getGlobalProperty name
  obj <- newObject >>= addOwnProperty "name" (VStr name)
                   >>= addOwnProperty "constructor" cstr
                   >>= objSetPrototype prototype
  errConstructor (VObj obj) [message]
  where name = T.pack $ show errorType

errFunction :: Shared JSObj -> JSVal -> [JSVal] -> Runtime JSVal
errFunction prototype _this args = do
  obj <- newObject >>= setClass "Error"
                   >>= objSetPrototype prototype
                   >>= objSetExtensible True
  errConstructor (VObj obj) args

errConstructor :: JSVal -> [JSVal] -> Runtime JSVal
errConstructor this args =
  let text = head args
  in case this of
    VObj obj -> do
      liftM VObj $ setClass "Error" obj
               >>= addOwnProperty "message" text
               >>= addOwnProperty "toString" (VNative "error/toString" 0 errorToString)
    _ -> raiseError "Bad this for Error constructor"

errorToString :: JSFunction
errorToString (VObj this) _args = do
  name <- dflt (VStr "Error") <$> objGet "name" this
  msg  <- objGet "message" this
  return $ VStr $ showVal name <> ": " <> showVal msg
  where
    dflt :: JSVal -> JSVal -> JSVal
    dflt d prop = case prop of
      VUndef -> d
      _      -> prop

errorToString _ _ = raiseError "errorToString called on non-object"
