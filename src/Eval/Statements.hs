{-# LANGUAGE LambdaCase, RankNTypes, GeneralizedNewtypeDeriving #-}

module Eval.Statements (evalCode, runCode, runProg, runExprStmt) where

import Control.Lens hiding (strict, Getter, Setter, op)
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
import Eval.Expressions
import Compiler
import Parse
import Expr
import JSNum
import Runtime
import Builtins
import Runtime
import Core

type StmtReturn = Result JSVal
data Result a = CTNormal   { rval :: Maybe a }
              | CTBreak    { rval :: Maybe a, label :: Maybe Ident }
              | CTContinue { rval :: Maybe a, label :: Maybe Ident }
              | CTReturn   { rval :: Maybe a }
              | CTThrow    { rval :: Maybe a }
              deriving (Show, Eq)



-- | Global interface to eval()
evalCode :: EvalCallType -> String -> Runtime JSVal
evalCode callType text = do
  currentStrictness <- return . cxtStrictness =<< getGlobalContext
  case parseJS'' text "(eval)" currentStrictness False of
    Left err -> raiseSyntaxError (show err)
    Right prog -> do
      result <- runInEvalContext callType prog
      case result of
        CTNormal (Just v) -> return v
        CTThrow  (Just v) -> do
          trace <- getStackTrace v
          throwError $ JSError (v, trace)
        _ -> return VUndef

-- ref 10.4.2
runInEvalContext :: EvalCallType -> Program -> Runtime StmtReturn
runInEvalContext callType (Program strictness body) = do
      let cxt = if callType == DirectEvalCall then getGlobalContext else initialCxt
      newCxt <- maybeNewContext strictness =<< cxt
      withNewContext newCxt $ do
        performDBI DBIEval strictness body
        withStrictness strictness (runStmt $ desugar body)

  where maybeNewContext NotStrict cxt = return cxt
        maybeNewContext Strict cxt = do
          rec <- createNewEnvRec
          env <- createNewEnv rec (lexEnv cxt)
          return cxt { lexEnv = env, varEnv = env, cxtStrictness = Strict }

-- | Global interface for running function bodies
runCode :: [Statement] -> Runtime (Either JSVal JSVal)
runCode body = do
  runStmts body >>= \case
    CTNormal _ -> return . Right $ VUndef
    CTThrow  v -> return . Left . fromMaybe VUndef $ v
    CTReturn v -> return . Right . fromMaybe VUndef $ v
    _ -> raiseError "Abnormal exit from function body"


runProg :: Program -> Runtime (Maybe JSVal)
runProg (Program strictness body) = do
  result <- withStrictness strictness (runStmt $ desugar body)
  case result of
    CTNormal v       -> return v
    CTThrow (Just v) -> throwValAsException v
    _                -> do
      liftIO $ putStrLn $ "Abnormal exit: " ++ show result
      return Nothing

  where
    throwValAsException :: JSVal -> Runtime a
    throwValAsException v = stringifyException v >>= throwError

-- ref 12.1
runStmts :: [Statement] -> Runtime StmtReturn
runStmts = runStmt . desugar



newtype StmtT r a = StmtT {
  unStmtT :: CoreStatement
             -> (a           -> r) -- normal
             -> (Maybe Label -> r) -- break
             -> (Maybe Label -> r) -- continue
             -> (      JSVal -> r) -- return
             -> (      JSVal -> r) -- throw
             -> r
}

instance Functor (StmtT r) where
  fmap f a = StmtT $ \s n b c r t ->
    let n' a' = n (f a') in unStmtT a s n' b c r t

instance Applicative (StmtT r) where
  pure = stmtReturn
  f <*> a = StmtT $ \s n b c r t ->
    unStmtT f s (\f' -> unStmtT a s (\a' -> n (f' a')) b c r t) b c r t

instance Monad (StmtT r) where
  return = stmtReturn
  (>>=)  = stmtBind

-- catchBreak :: StmtT r a -> (Maybe Label -> StmtT r a) -> StmtT r a
-- catchBreak a handler = StmtT $ \s n b c r t -> unStmtT a s n (\l -> handler l) c r t
-- catchContinue :: StmtT r a -> (Maybe Label -> StmtT r a) -> StmtT r a
-- catchContinue a handler = StmtT $ \s n b c r t -> unStmtT a s n b (\l -> handler l) r t
-- catchReturn :: StmtT r a -> (JSVal -> StmtT r a) -> StmtT r a
-- catchReturn a handler = StmtT $ \s n b c r t -> unStmtT a s n b c (\v -> handler v) t
-- catchThrow :: StmtT r a -> (JSVal -> StmtT r a) -> StmtT r a
-- catchThrow a handler = StmtT $ \s n b c r t -> unStmtT a s n b c r (\v -> handler v)


stmtReturn :: a -> StmtT r a
stmtReturn a = StmtT $ \s n _ _ _ _ -> n a

stmtBind :: StmtT r a -> (a -> StmtT r b) -> StmtT r b
stmtBind f g = StmtT $ \s n b c r t ->
  unStmtT f s (\a -> unStmtT (g a) s (\v -> n v) b c r t) b c r t

runStmtT :: CoreStatement -> StatementAction -> Runtime StmtReturn
runStmtT s a = unStmtT a s nor brk con ret thr
  where nor v = return . CTNormal           $ v
        brk l = return . CTBreak Nothing    $ l
        con l = return . CTContinue Nothing $ l
        ret v = return . CTReturn . Just    $ v
        thr v = return . CTThrow  . Just    $ v

type StatementAction = StmtT (Runtime StmtReturn) (Maybe JSVal)

sadapt :: Runtime StmtReturn -> StatementAction
sadapt a = StmtT $ \s nor brk con ret thr -> do
  r <- a `catchError` returnThrow s
  case r of
    CTNormal v -> nor v
    CTBreak _ l -> brk l
    CTContinue _ l -> con l
    CTReturn v -> ret (fromMaybe VUndef v)
    CTThrow v  -> thr (fromMaybe VUndef v)

returnThrow :: CoreStatement -> JSError -> Runtime StmtReturn
returnThrow s err = do
  val <- exceptionToVal (sourceLocation s :) err
  return $ CTThrow (Just val)

type Adapt = Runtime StmtReturn -> StatementAction

runStmt :: CoreStatement -> Runtime StmtReturn
runStmt stmt = runStmtT stmt (coreAction stmt)

coreAction :: CoreStatement -> StatementAction
coreAction stmt = case stmt of
      CoreBind dbit et bindings body -> {-# SCC coreBind #-}   runCoreBind    sadapt dbit et bindings body
      CoreBlock body                 -> {-# SCC coreBlock #-}  runCoreBlock   sadapt body
      CoreExpr _loc e                -> {-# SCC coreExpr #-}   runCoreExpr    sadapt e
      CoreIf _loc e ifThen ifElse    -> {-# SCC coreIf #-}     runCoreIf      sadapt e ifThen ifElse
      CoreLoop _loc test inc body e2 -> {-# SCC coreLoop #-}   runCoreLoop    sadapt test inc body e2
      CoreForIn _loc e1 e2 body      -> {-# SCC coreLoop #-}   runCoreForIn   sadapt e1 e2 body
      CoreBreak _loc lab             -> {-# SCC coreBreak #-}  runCoreBreak   lab
      CoreCont _loc lab              -> {-# SCC coreCont #-}   runCoreCont    lab
      CoreRet _loc expr              -> {-# SCC coreRet #-}    runCoreRet     expr
      CoreThrow _loc expr            -> {-# SCC coreThrow #-}  runCoreThrow   expr
      CoreCase _loc e cases          -> {-# SCC coreCase #-}   runCoreCase    sadapt e cases
      CoreLabel _loc lab body        -> {-# SCC coreLabel #-}  runCoreLabel   sadapt lab body
      CoreTry _loc body catch fin    -> {-# SCC coreTry #-}    runCoreTry     body catch fin

runCoreBind :: Adapt -> DBIType -> EnvType -> [(Ident, Expr)] -> CoreStatement -> StatementAction
runCoreBind sadapt dbiType envType bindings body = sadapt $ case envType of
  DeclarativeEnv -> do
    bindAll dbiType NotStrict bindings
    runStmt body

  ObjectEnv e -> do
    val <- runExprStmt e
    obj <- toObject =<< getValue val
    oldEnv <- lexEnv <$> getGlobalContext
    newEnv <- newObjectEnvironment obj (Just oldEnv) True
    withLexEnv newEnv $ runStmt body

runCoreBlock :: Adapt -> [CoreStatement] -> StatementAction
runCoreBlock sadapt body = sadapt $ runAll (CTNormal Nothing) body
  where
    runAll :: StmtReturn -> [CoreStatement] -> Runtime StmtReturn
    runAll r [] = return r
    runAll _ (s:rest) = do
      result <- runStmt s
      case result of
        CTNormal _ -> runAll result rest
        _          -> return result

runCoreExpr :: Adapt -> CompiledExpr -> StatementAction
runCoreExpr sadapt e = sadapt $ CTNormal . Just <$> (runExprStmt e >>= getValue)

runCoreIf :: Adapt -> CompiledExpr -> CoreStatement -> Maybe CoreStatement -> StatementAction
runCoreIf sadapt e ifThen ifElse = sadapt $ do
  cond <- toBoolean <$> (runExprStmt e >>= getValue)
  case (cond, ifElse) of
    (True, _)        -> runStmt ifThen
    (False, Just s)  -> runStmt s
    (False, Nothing) -> return (CTNormal Nothing)

runCoreLoop :: Adapt -> CompiledExpr -> CompiledExpr -> CoreStatement -> CompiledExpr -> StatementAction
runCoreLoop sadapt test inc body postTest = sadapt $ keepGoing Nothing where
  condition = toBoolean <$> (runExprStmt test >>= getValue)
  increment = runExprStmt inc >>= getValue
  keepGoing v = do
    willEval <- condition
    if not willEval
    then return $ CTNormal v
    else do
      r <- {-# SCC loop_body #-} runStmt body
      let v' = rval r <|> v
      case r of
        CTBreak    _ (Just l) -> ifCurrentLabel l (return r) (return $ CTNormal v')
        CTBreak    _ Nothing  -> return (CTNormal v')
        CTContinue _ (Just l) -> increment >> next v'
        CTContinue _ Nothing  -> increment >> next v'
        CTNormal   _          -> increment >> next v'
        _                     -> return r
  next v = do
    shouldContinue <- toBoolean <$> (runExprStmt postTest >>= getValue)
    if shouldContinue
    then keepGoing v
    else return $ CTNormal v

runCoreBreak :: Maybe Label -> StatementAction
runCoreBreak label = StmtT $ \_ _ b _ _ _ -> b label

runCoreCont :: Maybe Label -> StatementAction
runCoreCont label = StmtT $ \_ _ _ c _ _ -> c label

runCoreRet :: CompiledExpr -> StatementAction
runCoreRet expr = StmtT $ \_ _ _ _ r _ -> runExprStmt expr >>= getValue >>= r

runCoreThrow :: CompiledExpr -> StatementAction
runCoreThrow expr = StmtT $ \_ _ _ _ _ t -> runExprStmt expr >>= getValue >>= t

runCoreCase :: Adapt -> CompiledExpr -> [(Maybe CompiledExpr, CoreStatement)] -> StatementAction
runCoreCase sadapt scrutinee cases = sadapt $
  runExprStmt scrutinee >>= getValue >>= go cases
    where
      go :: [(Maybe CompiledExpr, CoreStatement)] -> JSVal -> Runtime StmtReturn
      go cs input = case cs of
        []       -> endWith dflt
        (c:rest) -> case c of
          (Nothing, _) -> go rest input
          (Just e,  s) -> do
            clauseSelector <- runExprStmt e >>= getValue
            if input `eqv` clauseSelector
            then endWith (s : map snd rest)
            else go rest input

      endWith = runToEnd Nothing

      runToEnd v [] = return (CTNormal v)
      runToEnd v (s:rest) = do
        r <- runStmt s
        let v' = rval r <|> v
        case r of
          CTNormal _  -> runToEnd v' rest
          CTBreak _ _ -> return $ CTNormal v'
          _           -> return $ r { rval = v' }

      dflt = map snd . dropWhile (isJust . fst) $ cases

runCoreLabel :: Adapt -> Label -> CoreStatement -> StatementAction
runCoreLabel sadapt lab body = StmtT $ \stmt nor brk con ret thr -> do
  result <- pushLabel lab $ runStmt body
  case result of
    CTBreak v l ->
      if l == Just lab
      then nor v
      else return result
    _ -> return result

runCoreTry :: CoreStatement -> Maybe (Ident, CoreStatement) -> Maybe CoreStatement -> StatementAction
runCoreTry body catch finally = StmtT $ \stmt nor brk con ret thr -> do
  br <- runStmt body `catchError` returnThrow stmt

  case (catch, finally) of
    (Just c, Nothing) -> do
      case br of
        CTThrow (Just exc) -> runCatch c exc
        _                  -> return br

    (Nothing, Just f) -> do
      fr <- runStmt f
      case fr of
        CTNormal _ -> return br
        _          -> return fr

    (Just c, Just f) -> do
      cr <- case br of
              CTThrow (Just exc) -> runCatch c exc
              _                  -> return br
      fr <- runStmt f
      case fr of
        CTNormal _ -> return cr
        _          -> return fr

    (Nothing, Nothing) -> return br

-- ref 12.14
runCatch :: (Ident, CoreStatement) -> JSVal -> Runtime StmtReturn
runCatch (var, block) c = do
  oldEnv <- lexEnv <$> getGlobalContext
  catchEnv <- newDeclarativeEnvironment (Just oldEnv)
  rec <- envRec <$> deref catchEnv
  createMutableBinding var True rec
  setMutableBinding var c False rec
  withLexEnv catchEnv $ runStmt block


-- ref 12.6.4
runCoreForIn :: Adapt -> CompiledExpr -> CompiledExpr -> CoreStatement -> StatementAction
runCoreForIn sadapt lhs e stmt = sadapt $ do
  exprRef <- runExprStmt e
  exprValue <- getValue exprRef
  if (exprValue == VNull || exprValue == VUndef)
  then return $ CTNormal Nothing
  else do
    obj <- toObject exprValue
    keys <- propMapKeys . view ownProperties <$> deref obj
    keepGoing obj keys Nothing where
      keepGoing :: Shared JSObj -> [String] -> Maybe JSVal -> Runtime StmtReturn
      keepGoing _ [] v = return $ CTNormal v
      keepGoing obj (p:ps) v = do
        desc <- objGetOwnProperty p obj
        let want = maybe False propIsEnumerable desc
        if want
        then do
          lhsRef <- runExprStmt lhs
          putValue' lhsRef (VStr p)
          s <- runStmt stmt
          let nextv = rval s <|> v
          case s of
            CTBreak _ _    -> return $ CTNormal nextv
            CTContinue _ _ -> keepGoing obj ps  nextv
            CTNormal _     -> keepGoing obj ps  nextv
            _ -> return s
        else keepGoing obj ps v

runExprStmt :: CompiledExpr -> Runtime JSVal
runExprStmt e = withEmptyStack $ evalExpr runExprStmt' e

withEmptyStack :: Runtime JSVal -> Runtime JSVal
withEmptyStack action = do
  oldStack <- use valueStack
  valueStack .= []
  result <- action
  stack <- use valueStack
  valueStack .= oldStack
  return result
  -- if null stack
  -- then return result
  -- else error $ "Stack should have been empty: is " ++ show stack


runExprStmt' :: Expr -> Runtime JSVal
runExprStmt' expr = undefined


createNewEnv :: EnvRec -> JSEnv -> Runtime (Shared LexEnv)
createNewEnv rec parent = do
  shareLexEnv $ LexEnv rec (Just parent)

createNewEnvRec :: Runtime EnvRec
createNewEnvRec = do
  m <- sharePropertyMap emptyPropMap
  return (DeclEnvRec m)
