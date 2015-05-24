module Runtime.Global where

import Control.Monad.State
import Runtime.Types
import Expr

emptyGlobal :: JSGlobal
emptyGlobal = JSGlobal Nothing Nothing Nothing Nothing

type SourceName = String
type SourceCode = String
jsEvalCode :: String -> JSRuntime StmtReturn
jsEvalCode str = do
  global <- get
  maybe (raiseError "Global evaluator not set up") ($ str) (globalEvaluator global)

jsRunStmts :: JSCxt -> [Statement] -> JSRuntime StmtReturn
jsRunStmts cxt stmts = do
  global <- get
  maybe (raiseError "Global runStmts not set up") invokeIt (globalRun global)
    where invokeIt run = run cxt stmts

getGlobalObject :: JSRuntime (Shared JSObj)
getGlobalObject = do
  global <- get
  case globalObject global of
    Nothing -> raiseError "No global object"
    Just obj -> return obj

getGlobalObjectPrototype :: JSRuntime (Shared JSObj)
getGlobalObjectPrototype = do
  global <- get
  case globalObjectPrototype global of
    Nothing -> raiseError "No global object"
    Just obj -> return obj
