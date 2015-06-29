module Runtime.Global where

import Control.Lens
import Control.Monad.Reader
import qualified Data.Map as M
import Data.Maybe
import Runtime.Types
import Runtime.PropMap
import Expr

type SourceName = String
type SourceCode = String

jsEvalCode :: EvalCallType -> String -> Runtime JSVal
jsEvalCode callType str = do
  f <- asks globalEvaluator
  f callType str

jsRunStmts :: [Statement] -> Runtime (Either JSVal JSVal)
jsRunStmts stmts = do
  f <- asks globalRun
  f stmts

getGlobalObject :: Runtime (Shared JSObj)
getGlobalObject = asks globalObject

getGlobalObjectPrototype :: Runtime (Shared JSObj)
getGlobalObjectPrototype = asks globalObjectPrototype

getGlobalContext :: Runtime JSCxt
getGlobalContext = asks globalContext

getGlobalStrictness :: Runtime Strictness
getGlobalStrictness = cxtStrictness <$> getGlobalContext

getGlobalEnvironment :: Runtime (Shared LexEnv)
getGlobalEnvironment = asks globalEnvironment

withGlobalContext :: (JSCxt -> JSCxt) -> Runtime a -> Runtime a
withGlobalContext f action = do
  oldContext <- asks globalContext
  local (\g -> g { globalContext = f oldContext }) action

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

pushLabel :: Label -> Runtime a -> Runtime a
pushLabel label action = local (over globalLabelStack (label:)) action

ifCurrentLabel :: Label -> Runtime a -> Runtime a -> Runtime a
ifCurrentLabel label onStack notOnStack = do
  stack <- view globalLabelStack <$> ask
  if label `elem` stack
  then onStack
  else notOnStack
