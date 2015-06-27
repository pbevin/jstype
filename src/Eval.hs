{-# LANGUAGE LambdaCase #-}

module Eval (doJS, runJS, evalJS, jsEvalExpr, runtime, runtime', toRuntimeError, RuntimeError(..)) where

import Control.Lens
import Control.Monad.State
import Control.Monad.Except
import Control.Monad.Writer
import Control.Applicative
import Control.Arrow
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
  result <- runtime $ (runExprStmt (parseExpr input) >>= getValue) `catchError` rethrowAsString
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

evalCode :: EvalCallType -> String -> Runtime StmtReturn
evalCode callType text = do
  currentStrictness <- return . cxtStrictness =<< getGlobalContext
  case parseJS'' text "(eval)" currentStrictness False of
    Left err -> raiseSyntaxError (show err)
    Right prog -> runInEvalContext callType prog

-- ref 10.4.2
runInEvalContext :: EvalCallType -> Program -> Runtime StmtReturn
runInEvalContext callType (Program strictness stmts) = do
      let cxt = if callType == DirectEvalCall then getGlobalContext else initialCxt
      newCxt <- maybeNewContext strictness =<< cxt
      withNewContext newCxt $ do
        performDBI DBIEval strictness stmts
        withStrictness strictness (runStmts stmts)

  where maybeNewContext NotStrict cxt = return cxt
        maybeNewContext Strict cxt = do
          rec <- createNewEnvRec
          env <- createNewEnv rec (lexEnv cxt)
          return cxt { lexEnv = env, varEnv = env, cxtStrictness = Strict }

runJS' :: String -> String -> IO (Either JSError (Maybe JSVal), String)
runJS' sourceName input =
  case parseJS' input sourceName of
    Left err -> return (Left $ JSError (VStr $ "SyntaxError: " ++ show err, []), "")
    Right ast -> doJS (runProg ast)

doJS :: Runtime a -> IO (Either JSError a, String)
doJS action = do
  let (r,s) = initialState
  (result, _, stdout) <- runRuntime r s (bootstrap >> action)
  return (result, stdout)

runtime :: Runtime a -> IO (Either JSError a)
runtime p =
  let (r, s) = initialState
  in do (result, _, _) <- runRuntime r s (bootstrap >> p)
        return result

runtime' :: Runtime a -> IO (Either JSError a)
runtime' = runtime

initialState :: (JSGlobal, Store)
initialState = (globals, store)
  where
    globals = JSGlobal (s 1) (s 2) evalCode runStmts (e 3) cxt
    store   = Store 100 objects pms envs

    object  = obj { _objPrototype = Just (s 2) }
    proto   = obj
    env     = LexEnv (ObjEnvRec (s 1) False) Nothing
    cxt     = JSCxt (e 3) (e 3) (VObj $ s 1) NotStrict

    objects = M.fromList [ (1, object), (2, proto) ]
    pms     = M.fromList [ ]
    envs    = M.fromList [ (3, env) ]

    s oid    = Shared (storeObjStore.at oid) oid
    e oid    = Shared (storeLexEnvStore.at oid) oid

    obj     = emptyObject { _defineOwnPropertyMethod = Just objDefineOwnPropertyObject,
                            _getOwnPropertyMethod = Just objGetOwnPropertyObj,
                            _getMethod = Just objGetObj }

bootstrap :: Runtime ()
bootstrap = buildGlobalObject >> configureBuiltins
