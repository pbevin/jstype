{-# LANGUAGE LambdaCase, RankNTypes, GeneralizedNewtypeDeriving, OverloadedStrings #-}

module Eval.Statements (evalCode, runCode, runProg, runExprStmt) where

import Control.Lens hiding (strict, Getter, Setter, op)
import Control.Monad.State
import Control.Monad.Except
import Control.Monad.Writer
import Control.Applicative
import Control.Arrow
import Data.Maybe
import Data.List (intercalate, nub)
import Data.Bits
import Data.Foldable
import qualified Data.Text as T
import Data.Text (Text)
import Text.Show.Functions
import Eval.Expressions
import Compiler
import Expr
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
evalCode :: EvalCallType -> Text -> Runtime JSVal
evalCode callType str = do
  currentStrictness <- case callType of
    DirectEvalCall -> return . cxtStrictness =<< getGlobalContext
    IndirectEvalCall -> return NotStrict
  case parseJS'' str "(eval)" currentStrictness False of
    Left err -> raiseSyntaxError (T.pack $ show err)
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
    x -> raiseError . T.pack $ "Abnormal exit from function body " ++ show x


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

data Cont r = Cont {
  stmt :: CoreStatement,
  val :: Maybe JSVal,
  nor :: (Maybe JSVal ->                r), -- normal
  brk :: (Maybe JSVal -> Maybe Label -> r), -- break
  con :: (Maybe JSVal -> Maybe Label -> r), -- continue
  ret :: (Maybe JSVal ->                r), -- return
  thr :: (Maybe JSVal ->                r)  -- throw
}

type StatementAction = Cont (Runtime StmtReturn) -> Runtime StmtReturn

passToThrow :: JSError -> StatementAction
passToThrow err cont = do
  val <- exceptionToVal (sourceLocation (stmt cont) :) err
  thr cont (Just val)


returnThrow :: CoreStatement -> JSError -> Runtime StmtReturn
returnThrow s err = do
  val <- exceptionToVal (sourceLocation s :) err
  return $ CTThrow (Just val)

runStmt :: CoreStatement -> Runtime StmtReturn
runStmt stmt = runS stmt cont `catchError` returnThrow stmt
  where n v   = return . CTNormal           $ v
        b v l = return . CTBreak v          $ l
        c v l = return . CTContinue v       $ l
        r v   = return . CTReturn           $ v
        t v   = return . CTThrow            $ v
        cont  = Cont stmt Nothing n b c r t

sadapt :: Runtime StmtReturn -> StatementAction
sadapt r cont = r >>= \case
  CTNormal v -> nor cont v
  CTBreak v l -> brk cont v l
  CTContinue v l -> con cont v l
  CTReturn v -> ret cont v
  CTThrow v -> thr cont v


runS :: CoreStatement -> StatementAction
runS s cont = coreAction s $ cont { stmt = s }

coreAction :: CoreStatement -> StatementAction
coreAction stmt = case stmt of
      CoreBind dbit et bindings body -> {-# SCC coreBind #-}   runCoreBind    dbit et bindings body
      CoreBlock body                 -> {-# SCC coreBlock #-}  runCoreBlock   body
      CoreExpr _loc e                -> {-# SCC coreExpr #-}   runCoreExpr    e
      CoreIf _loc e ifThen ifElse    -> {-# SCC coreIf #-}     runCoreIf      e ifThen ifElse
      CoreLoop loc test inc body e2  -> {-# SCC coreLoop #-}   runCoreLoop    (srcLabel loc) test inc body e2
      CoreForIn _loc e1 e2 body      -> {-# SCC coreLoop #-}   runCoreForIn   e1 e2 body
      CoreBreak _loc lab             -> {-# SCC coreBreak #-}  runCoreBreak   lab
      CoreCont _loc lab              -> {-# SCC coreCont #-}   runCoreCont    lab
      CoreRet _loc expr              -> {-# SCC coreRet #-}    runCoreRet     expr
      CoreThrow _loc expr            -> {-# SCC coreThrow #-}  runCoreThrow   expr
      CoreCase _loc e cases          -> {-# SCC coreCase #-}   runCoreCase    e cases
      CoreLabel _loc lab body        -> {-# SCC coreLabel #-}  runCoreLabel   lab body
      CoreTry _loc body catch fin    -> {-# SCC coreTry #-}    runCoreTry     body catch fin

runCoreBind :: DBIType -> EnvType -> [(Ident, Expr)] -> CoreStatement -> StatementAction
runCoreBind dbiType envType bindings body cont = case envType of
  DeclarativeEnv -> do
    bindAll dbiType NotStrict bindings
    runS body cont

  ObjectEnv e -> do
    val <- runExprStmt e
    obj <- toObject =<< getValue val
    oldEnv <- lexEnv <$> getGlobalContext
    newEnv <- newObjectEnvironment obj (Just oldEnv) True
    sadapt (withLexEnv newEnv $ runStmt body) cont

runCoreBlock :: [CoreStatement] -> StatementAction
runCoreBlock body cont = runAll Nothing body
  where
    runAll :: Maybe JSVal -> [CoreStatement] -> Runtime StmtReturn
    runAll v [] = nor cont v
    runAll v (s:rest) = runS s cont'
      where
        cont' = cont { nor = \v' -> runAll (v' <|> v) rest,
                       brk = \v' l -> brk cont (v' <|> v) l,
                       con = \v' l -> con cont (v' <|> v) l,
                       ret = \v'   -> ret cont (v' <|> v),
                       thr = \v'   -> thr cont (v' <|> v) }

runCoreExpr :: CompiledExpr -> StatementAction
runCoreExpr e cont = do
  (runExprStmt e >>= getValue >>= passNormal) `catchError` \e -> passToThrow e cont
    where passNormal v = nor cont (Just v)

runCoreIf :: CompiledExpr -> CoreStatement -> Maybe CoreStatement -> StatementAction
runCoreIf e ifThen ifElse cont = do
  cond <- toBoolean <$> (runExprStmt e >>= getValue)
  case (cond, ifElse) of
    (True, _)        -> runS ifThen cont
    (False, Just s)  -> runS s cont
    (False, Nothing) -> nor cont Nothing

runCoreLoop :: [Label] -> CompiledExpr -> CompiledExpr -> CoreStatement -> Maybe CompiledExpr -> StatementAction
runCoreLoop labelSet test inc body postTest cont = do
  keepGoing labelSet Nothing where
    condition = toBoolean <$> (runExprStmt test >>= getValue)
    increment = runExprStmt inc >>= getValue
    keepGoing labelSet v = do
      willEval <- condition
      if not willEval
      then nor cont v
      else {-# SCC loop_body #-} runS body cont { nor = nor', brk = brk', con = con' }
        where nor' v' = nextIter v'
              brk' v' (Just l) = if l `elem` labelSet
                                 then breakOut v'
                                 else brk cont (v' <|> v) (Just l)
              brk' v' Nothing  = breakOut v'
              con' v' (Just l) = if l `elem` labelSet
                                 then nextIter v'
                                 else con cont (v' <|> v) (Just l)
              con' v' Nothing  = nextIter v'

              nextIter v' = increment >> next labelSet (v' <|> v)
              breakOut v' = nor cont (v' <|> v)

    next labelSet v = do
      case postTest of
        Nothing -> keepGoing labelSet v
        Just tst ->  do
          shouldContinue <- toBoolean <$> (runExprStmt tst >>= getValue)
          if shouldContinue
          then keepGoing labelSet v
          else nor cont v

runCoreBreak :: Maybe Label -> StatementAction
runCoreBreak label cont = brk cont Nothing label

runCoreCont :: Maybe Label -> StatementAction
runCoreCont label cont = con cont Nothing label

runCoreRet :: CompiledExpr -> StatementAction
runCoreRet expr cont = runCoreExpr expr $ cont { nor = \v -> ret cont v }

runCoreThrow :: CompiledExpr -> StatementAction
runCoreThrow expr cont = runCoreExpr expr $ cont { nor = \v -> thr cont v }

runCoreCase :: CompiledExpr -> [(Maybe CompiledExpr, CoreStatement)] -> StatementAction
runCoreCase scrutinee cases cont =
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

      runToEnd v [] = nor cont v
      runToEnd v (s:rest) = do
        r <- runStmt s
        let v' = rval r <|> v
        case r of
          CTNormal _  -> runToEnd v' rest
          CTBreak _ _ -> nor cont v'
          CTContinue _ l -> con cont v' l
          CTReturn _ -> ret cont v'
          CTThrow _ -> thr cont v'

      dflt = map snd . dropWhile (isJust . fst) $ cases

runCoreLabel :: Label -> CoreStatement -> StatementAction
runCoreLabel lab body cont =
  runS body $ cont { brk = brk' }
    where brk' v l = if l == Just lab
                     then nor cont v
                     else brk cont v l

runCoreTry :: CoreStatement -> Maybe (Ident, CoreStatement) -> Maybe CoreStatement -> StatementAction
runCoreTry body catch finally cont = runS body cont' `catchError` \e -> passToThrow e cont'
  where
    cont' = case (catch, finally) of
      (Just c, Nothing) -> contC c
      (Nothing, Just f) -> contF f
      (Just c, Just f)  -> contCF c f

    contC c    = cont { thr = \(Just exc) -> runCatch c exc cont }

    contF f    = cont { nor = \v   -> runS f cont { nor = const $ nor cont v },
                        brk = \v l -> runS f cont { nor = const $ brk cont v l },
                        con = \v l -> runS f cont { nor = const $ con cont v l },
                        ret = \v   -> runS f cont { nor = const $ ret cont v },
                        thr = \v   -> runS f cont { nor = const $ thr cont v } }

    contCF c f = cont { nor = \v   -> runS f cont { nor = const $ nor cont v },
                        brk = \v l -> runS f cont { nor = const $ brk cont v l },
                        con = \v l -> runS f cont { nor = const $ con cont v l },
                        ret = \v   -> runS f cont { nor = const $ ret cont v },
                        thr = \(Just exc) -> runCatch c exc (contF f) }

-- ref 12.14
runCatch :: (Ident, CoreStatement) -> JSVal -> StatementAction
runCatch (var, block) c cont = do
  -- debugVal c
  oldEnv <- lexEnv <$> getGlobalContext
  catchEnv <- newDeclarativeEnvironment (Just oldEnv)
  rec <- envRec <$> deref catchEnv
  createMutableBinding var True rec
  setMutableBinding var c False rec
  sadapt (withLexEnv catchEnv $ runStmt block) cont

-- ref 12.6.4
runCoreForIn :: CompiledExpr -> CompiledExpr -> CoreStatement -> StatementAction
runCoreForIn lhs e stmt cont = do
  exprRef <- runExprStmt e
  exprValue <- getValue exprRef
  if (exprValue == VNull || exprValue == VUndef)
  then nor cont Nothing
  else do
    obj <- toObject exprValue
    keys <- nub <$> recursivelyFindKeys obj
    keepGoing obj keys Nothing cont where
      keepGoing :: Shared JSObj -> [Text] -> Maybe JSVal -> StatementAction
      keepGoing _ [] v cont = nor cont v
      keepGoing obj (p:ps) v cont = do
        desc <- objGetProperty p obj
        let want = maybe False propIsEnumerable desc
        if want
        then do
          lhsRef <- runExprStmt lhs
          putValue' lhsRef (VStr p)
          s <- runStmt stmt
          let nextv = rval s <|> v
          case s of
            CTBreak _ _    -> nor cont nextv
            CTContinue _ _ -> keepGoing obj ps nextv cont
            CTNormal _     -> keepGoing obj ps nextv cont
            _ -> sadapt (return s) cont
        else keepGoing obj ps v cont

    -- propMapKeys . view ownProperties <$> deref obj

recursivelyFindKeys :: Shared JSObj -> Runtime [Text]
recursivelyFindKeys obj = do
  myKeys <- propMapKeys . view ownProperties <$> deref obj
  proto <- view objPrototype <$> deref obj
  case proto of
    Nothing -> return myKeys
    Just p  -> (myKeys ++) <$> recursivelyFindKeys p

runExprStmt :: CompiledExpr -> Runtime JSVal
runExprStmt = evalExpr

createNewEnv :: EnvRec -> JSEnv -> Runtime (Shared LexEnv)
createNewEnv rec parent = do
  shareLexEnv $ LexEnv rec (Just parent)

createNewEnvRec :: Runtime EnvRec
createNewEnvRec = do
  m <- sharePropertyMap emptyPropMap
  return (DeclEnvRec m)
