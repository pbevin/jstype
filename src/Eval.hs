{-# LANGUAGE LambdaCase #-}

module Eval (runJS, evalJS, jsEvalExpr, runtime, runtime', RuntimeError(..)) where

import Control.Monad.State
import Control.Monad.Except
import Control.Monad.Writer
import Control.Applicative
import Control.Arrow
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
    ((Left err, _), _) -> return $ Left $ toRuntimeError err
    ((Right v, _), _)  -> return $ Right v

runJS :: String -> String -> IO (Either RuntimeError String)
runJS sourceName input = do
  runJS' sourceName input >>= \case
    ((Left err, _), _)     -> return $ Left $ toRuntimeError err
    ((Right _, output), _) -> return $ Right output

jsEvalExpr :: String -> IO JSVal
jsEvalExpr input = do
  result <- runtime $ do
    initGlobals evalCode
    (runExprStmt (parseExpr input) >>= getValue) `catchError` rethrowAsString
  case result of
    Left err  -> do
      error (show err)
    Right val -> return val

rethrowAsString :: JSError -> Runtime a
rethrowAsString err = case err of
  JSProtoError (etype, msg) -> throwError $ JSError (VStr $ show etype ++ ": " ++ msg, [])
  JSError (err, s) -> do
    v <- toString err
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
          rec <- newEnvRec
          env <- newEnv rec (lexEnv cxt)
          return cxt { lexEnv = env, varEnv = env, cxtStrictness = Strict }

runJS' :: String -> String -> IO ((Either JSError (Maybe JSVal), String), JSGlobal)
runJS' sourceName input = case parseJS' input sourceName of
  Left err -> return ((Left $ JSError (VStr $ "SyntaxError: " ++ show err, []), ""), emptyGlobal)
  Right ast -> runRuntime (initGlobals evalCode >> runProg ast)

runtime :: Runtime a -> IO (Either JSError a)
runtime p = do
  result <- runRuntime p
  return $ fst (fst result)

runtime' :: Runtime a -> IO (Either JSError a)
runtime' p = runtime (initGlobals evalCode >> p)

initGlobals :: (EvalCallType -> String -> Runtime StmtReturn) -> Runtime ()
initGlobals evalCode = do
  objProto <- createGlobalObjectPrototype
  modify $ \st -> st { globalObjectPrototype = Just objProto }

  newGlobalObject <- createGlobalThis
  modify $ \st -> st { globalObject = Just newGlobalObject,
                       globalEvaluator = Just evalCode,
                       globalRun = Just runStmts }
  cxt <- initialCxt
  modify $ \st -> st { globalContext = Just cxt,
                       globalEnvironment = Just (lexEnv cxt) }

  configureBuiltins newGlobalObject

