module Runtime.Error where

import Control.Monad.Except
import Control.Applicative
import Data.Maybe
import Runtime.Types
import Runtime.Object
import Runtime.Conversion




raise :: JSVal -> JSRuntime a
raise err = throwError (err, [])

raiseReferenceError :: String -> JSRuntime a
raiseReferenceError msg = createReferenceError msg >>= raise

createReferenceError :: String -> JSRuntime JSVal
createReferenceError message = do
  obj <- newObject >>= addOwnProperty "name" (VStr "ReferenceError")
  errConstructor (VObj obj) [VStr message]

createSyntaxError :: JSVal -> JSRuntime JSVal
createSyntaxError text =
  VObj <$> (newObject >>= addOwnProperty "name" (VStr "SyntaxError")
                      >>= addOwnProperty "message" text)


errFunction :: JSVal -> [JSVal] -> JSRuntime JSVal
errFunction this args = do
  className <- valClassName this
  obj <- newObject >>= setClass className
  errConstructor (VObj obj) args

errConstructor :: JSVal -> [JSVal] -> JSRuntime JSVal
errConstructor this args =
  let text = head args
  in case this of
    VObj obj -> do
      name <- objClassName obj
      setClass name obj >>= addOwnProperty "message" text
                        >>= addOwnProperty "toString" (VNative errorToString)
                        >>= return . VObj

errorToString :: JSFunction
errorToString (VObj this) _args = do
  obj  <- deref this
  name <- objGetProperty "name" obj >>= return . fromMaybe (VStr "Error")
  msg  <- objGetProperty "message" obj >>= return . fromMaybe VUndef
  return $ VStr $ showVal name ++ ": " ++ showVal msg
