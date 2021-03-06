module Runtime.Global where

import Control.Lens
import Control.Monad.Reader
import qualified Data.Map as M
import qualified Data.Text as T
import Data.Text (Text)
import Data.Maybe
import Runtime.Types
import Runtime.PropMap
import Expr

type SourceName = String
type SourceCode = String

jsEvalCode :: EvalCallType -> Text -> Runtime JSVal
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
