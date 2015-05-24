module Runtime.Global where

import GHC.Stack
import Control.Monad.State
import Data.Maybe
import Runtime.Types
import Expr

emptyGlobal :: JSGlobal
emptyGlobal = JSGlobal Nothing Nothing Nothing Nothing Nothing

type SourceName = String
type SourceCode = String
jsEvalCode :: String -> Runtime StmtReturn
jsEvalCode str = do
  global <- get
  maybe (raiseError "Global evaluator not set up") ($ str) (globalEvaluator global)

jsRunStmts :: JSCxt -> [Statement] -> Runtime StmtReturn
jsRunStmts cxt stmts = do
  global <- get
  maybe (raiseError "Global runStmts not set up") invokeIt (globalRun global)
    where invokeIt run = run stmts

getGlobalObject :: Runtime (Shared JSObj)
getGlobalObject = do
  global <- get
  case globalObject global of
    Nothing -> raiseError "No global object"
    Just obj -> return obj

getGlobalObjectPrototype :: Runtime (Shared JSObj)
getGlobalObjectPrototype = do
  global <- get
  case globalObjectPrototype global of
    Nothing -> raiseError "No global object"
    Just obj -> return obj

putGlobalContext :: JSCxt -> Runtime ()
putGlobalContext cxt = modify $ \g -> g { globalContext = Just cxt }

getGlobalContext :: Runtime JSCxt
getGlobalContext = get >>= maybe (errorWithStackTrace "no context") return . globalContext

withNewContext :: JSCxt -> Runtime a -> Runtime a
withNewContext cxt action = do
  oldContext <- getGlobalContext
  putGlobalContext cxt
  result <- action
  putGlobalContext oldContext
  return result

withStrictness :: Strictness -> Runtime a -> Runtime a
withStrictness strictness action = do
  oldContext <- getGlobalContext
  putGlobalContext $ oldContext { cxtStrictness = strictness }
  result <- action
  putGlobalContext oldContext
  return result
