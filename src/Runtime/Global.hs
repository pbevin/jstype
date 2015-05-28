module Runtime.Global where

import GHC.Stack
import Control.Monad.State
import Data.Maybe
import Runtime.Types
import Expr

type SourceName = String
type SourceCode = String
jsEvalCode :: String -> Runtime StmtReturn
jsEvalCode str = do
  global <- get
  maybe (raiseError "Global evaluator not set up") ($ str) (globalEvaluator global)

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

withGlobalContext :: (JSCxt -> JSCxt) -> Runtime a -> Runtime a
withGlobalContext f action = do
  oldContext <- globalContext <$> get
  modify $ \g -> g { globalContext = f <$> oldContext }
  result <- action
  modify $ \g -> g { globalContext = oldContext }
  return result

withNewContext :: JSCxt -> Runtime a -> Runtime a
withNewContext cxt = withGlobalContext (const cxt)

withStrictness :: Strictness -> Runtime a -> Runtime a
withStrictness strictness = withGlobalContext $ \cxt -> cxt { cxtStrictness = strictness }
