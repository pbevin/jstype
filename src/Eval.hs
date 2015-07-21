{-# LANGUAGE LambdaCase #-}

module Eval (doJS, runJS, evalJS, jsEvalExpr, runtime, runtime', toRuntimeError, RuntimeError(..)) where

import Control.Lens
import Control.Monad.State
import Control.Monad.Except
import Control.Monad.Writer
import Control.Applicative
import Control.Arrow
import Data.IORef
import qualified Data.Map as M
import Data.Maybe
import Data.List (intercalate)
import Data.Bits
import Data.Foldable
import Text.Show.Functions
import Parse
import Expr
import JSNum
import Runtime
import Builtins
import Eval.Statements
import Compiler
import Core

data RuntimeError = RuntimeError {
  errorMessage :: String,
  errorObject :: JSVal,
  errorStack :: [String]
} deriving Eq

instance Show RuntimeError where
  show (RuntimeError msg _ stack) =
    msg ++ "\n" ++ unlines (map ("    " ++) stack)



evalJS :: String -> String -> IO (Either RuntimeError (Maybe JSVal))
evalJS sourceName input = do
  runJS' sourceName input >>= \case
    (Left err, _) -> return $ Left $ toRuntimeError err
    (Right v, _)  -> return $ Right v

runJS :: String -> String -> IO (Either (String, RuntimeError) String)
runJS sourceName input = do
  runJS' sourceName input >>= \case
    (Left err, output)     -> return $ Left (output, toRuntimeError err)
    (Right _, output) -> return $ Right output

jsEvalExpr :: String -> IO JSVal
jsEvalExpr input = do
  result <- runtime $ (runExprStmt (compile $ parseExpr input) >>= getValue) `catchError` rethrowAsString
  case result of
    Left err  -> do
      error (show err)
    Right val -> return val

rethrowAsString :: JSError -> Runtime a
rethrowAsString err = case err of
  JSProtoError (etype, msg) -> throwError $ JSError (VStr $ show etype ++ ": " ++ msg, [])
  JSError (e, s) -> do
    v <- toString e
    throwError $ JSError (VStr v, s)

toRuntimeError :: JSError -> RuntimeError
toRuntimeError (JSError (VStr err, stack)) = RuntimeError err (VStr err) (reverse $ map show stack)
toRuntimeError e = error $ "Runtime did not convert error to string: " ++ show e

runJS' :: String -> String -> IO (Either JSError (Maybe JSVal), String)
runJS' sourceName input =
  case parseJS' input sourceName of
    Left err -> return (Left $ JSError (VStr $ "SyntaxError: " ++ show err, []), "")
    Right ast -> doJS (runProg ast)

doJS :: Runtime a -> IO (Either JSError a, String)
doJS action = do
  (r,s) <- initialState
  (result, _, stdout) <- runRuntime r s (bootstrap >> action)
  return (result, stdout)

runtime :: Runtime a -> IO (Either JSError a)
runtime p = do
  (r, s) <- initialState
  (result, _, _) <- runRuntime r s (bootstrap >> p)
  return result

runtime' :: Runtime a -> IO (Either JSError a)
runtime' = runtime

initialState :: IO (JSGlobal, Store)
initialState = do
  (pro, obj, env) <- createInitialSharedObjects

  let globals   = JSGlobal obj pro evalCode runCode env cxt
      store     = Store 4

      cxt       = JSCxt env env (VObj obj) NotStrict

  return (globals, store)

createInitialSharedObjects :: IO (Shared JSObj, Shared JSObj, Shared LexEnv)
createInitialSharedObjects =
  let obj       = emptyObject { _defineOwnPropertyMethod = Just objDefineOwnPropertyObject,
                                _getOwnPropertyMethod = Just objGetOwnPropertyObj,
                                _getMethod = Just objGetObj }
  in do proRef <- newIORef $ obj
        objRef <- newIORef $ obj { _objPrototype = Just $ Shared proRef 1 }
        envRef <- newIORef $ LexEnv (ObjEnvRec (Shared objRef 2) False) Nothing
        return (Shared proRef 1, Shared objRef 2, Shared envRef 3)

bootstrap :: Runtime ()
bootstrap = buildGlobalObject >> configureBuiltins
