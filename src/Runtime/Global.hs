module Runtime.Global where

import GHC.Stack
import Control.Monad.State
import qualified Data.Map as M
import Data.Maybe
import Runtime.Types
import Runtime.PropMap
import Expr

type SourceName = String
type SourceCode = String
jsEvalCode :: EvalCallType -> String -> Runtime StmtReturn
jsEvalCode callType str = do
  global <- get
  maybe (raiseError "Global evaluator not set up") eval (globalEvaluator global)
    where eval f = f callType str

jsRunStmts :: [Statement] -> Runtime StmtReturn
jsRunStmts stmts = do
  global <- get
  maybe (raiseError "Global runStmts not set up") invokeIt (globalRun global)
    where invokeIt run = run stmts

getGlobal :: String -> (JSGlobal -> Maybe a) -> Runtime a
getGlobal s f = do
  global <- get
  case f global of
    Nothing -> raiseError $ "Global " ++ s ++ " not configured"
    Just a -> return a


getGlobalObject :: Runtime (Shared JSObj)
getGlobalObject = getGlobal "obj" globalObject

getGlobalObjectPrototype :: Runtime (Shared JSObj)
getGlobalObjectPrototype = getGlobal "proto" globalObjectPrototype

getGlobalContext :: Runtime JSCxt
getGlobalContext = getGlobal "cxt" globalContext

getGlobalStrictness :: Runtime Strictness
getGlobalStrictness = cxtStrictness <$> getGlobalContext

getGlobalEnvironment :: Runtime (Shared LexEnv)
getGlobalEnvironment = getGlobal "environment" globalEnvironment

withGlobalContext :: (JSCxt -> JSCxt) -> Runtime a -> Runtime a
withGlobalContext f action = do
  oldContext <- globalContext <$> get
  modify $ \g -> g { globalContext = f <$> oldContext }
  result <- action `finally` (modify $ \g -> g { globalContext = oldContext })
  return result

withNewContext :: JSCxt -> Runtime a -> Runtime a
withNewContext cxt = withGlobalContext (const cxt)

ifStrictContext :: Runtime a -> Runtime ()
ifStrictContext a = do
  strictness <- cxtStrictness <$> getGlobalContext
  when (strictness == Strict) $ void a

withStrictness :: Strictness -> Runtime a -> Runtime a
withStrictness strictness = withGlobalContext $ \cxt -> cxt { cxtStrictness = strictness }

withLexEnv :: JSEnv -> Runtime a -> Runtime a
withLexEnv env = withGlobalContext $ \cxt -> cxt { lexEnv = env }
