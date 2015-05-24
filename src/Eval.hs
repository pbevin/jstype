{-# LANGUAGE LambdaCase #-}

module Eval (runJS, evalJS, jsEvalExpr, runtime, RuntimeError(..)) where

import Control.Monad.State
import Control.Monad.Except
import Control.Monad.Writer
import Control.Applicative
import Data.Maybe
import Data.List (intercalate)
import Data.Bits
import qualified Data.Foldable as F
import qualified Data.Map as M
import Text.Show.Functions
import Parse
import Expr
import JSNum
import Runtime

import Debug.Trace

data RuntimeError = RuntimeError {
  errorMessage :: String,
  errorStack :: [String]
} deriving (Show, Eq)

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
    initGlobals
    cxt <- initialCxt
    putGlobalContext cxt
    runExprStmt cxt (parseExpr input) >>= getValue
  case result of
    Left err  -> error (show err)
    Right val -> return val

toRuntimeError :: JSError -> RuntimeError
toRuntimeError (VStr err, stack) = RuntimeError err (map show stack)

evalCode :: String -> Runtime StmtReturn
evalCode text = do
  currentStrictness <- return . cxtStrictness =<< getGlobalContext
  case parseJS'' text "(eval)" currentStrictness of
    Left err -> raiseSyntaxError (show err)
    Right (Program strictness stmts) -> do
      runStmts stmts

runJS' :: String -> String -> IO ((Either JSError (Maybe JSVal), String), JSGlobal)
runJS' sourceName input = case parseJS' input sourceName of
  Left err -> return ((Left (VStr $ "SyntaxError: " ++ show err, []), ""), emptyGlobal)
  Right ast -> runRuntime (runProg ast)

runtime :: Runtime a -> IO (Either JSError a)
runtime p = do
  result <- runRuntime p
  return $ fst (fst result)


runRuntime :: Runtime a -> IO ((Either JSError a, String), JSGlobal)
runRuntime a = runStateT (runWriterT $ runExceptT $ unJS a) emptyGlobal

initGlobals :: Runtime ()
initGlobals = do
  objProto <- createGlobalObjectPrototype
  modify $ \st -> st { globalObjectPrototype = Just objProto }

  newGlobalObject <- createGlobalThis
  modify $ \st -> st { globalObject = Just newGlobalObject,
                       globalEvaluator = Just evalCode,
                       globalRun = Just runStmts }


runProg :: Program -> Runtime (Maybe JSVal)
runProg (Program strictness stmts) = do
  initGlobals
  cxt <- initialCxt
  putGlobalContext cxt
  result <- withStrictness strictness (runStmts stmts)
  case result of
    (CTNormal, v, _) -> return v
    (CTThrow, Just v, _) -> do
      v' <- callToString cxt v
      throwError (VStr $ show v', [])
    _ -> do
      liftIO $ putStrLn $ "Abnormal exit: " ++ show result
      return Nothing

callToString :: JSCxt -> JSVal -> Runtime JSVal
callToString _ (VStr str) = return (VStr str)
callToString cxt v = do
  obj <- toObject cxt v
  ref <- memberGet obj "toString"
  funCall cxt ref []



debug :: Show a => a -> Runtime ()
debug a = do
  liftIO $ print a

returnThrow :: Statement -> JSError -> Runtime StmtReturn
returnThrow s (err, trace) = do
  setStacktrace err (sourceLocation s : trace)
  return (CTThrow, Just err, Nothing)

setStacktrace :: JSVal -> [SrcLoc] -> Runtime ()
setStacktrace v@(VObj objRef) stack = do
  void $ addOwnProperty "stack" (VStacktrace stack) objRef
setStacktrace x stack = return ()


runStmts :: [Statement] -> Runtime StmtReturn
runStmts = runStmts' (CTNormal, Nothing, Nothing) where
  runStmts' emptyResult [] = return emptyResult
  runStmts' _ (s:stmts) = do
    result <- runStmt s `catchError` returnThrow s
    case result of
      (CTNormal, _, _) -> runStmts' result stmts
      _ -> return result

runStmt :: Statement -> Runtime StmtReturn
runStmt s = do { cxt <- getGlobalContext; case s of
  ExprStmt loc e -> do
    val <- runExprStmt cxt e >>= getValue
    return (CTNormal, Just val, Nothing)

  VarDecl loc assignments -> do
    F.forM_ assignments $ \(x, e) -> case e of
      Nothing  -> putVar cxt x VUndef
      Just e' -> do
        v <- getValue =<< runExprStmt cxt e'
        putVar cxt x v
    return (CTNormal, Nothing, Nothing)

  LabelledStatement loc label stmt -> runStmt stmt

  IfStatement loc predicate ifThen ifElse -> do -- ref 12.5
    v <- runExprStmt cxt predicate >>= getValue
    if toBoolean v
    then runStmt ifThen
    else case ifElse of
           Nothing -> return (CTNormal, Nothing, Nothing)
           Just s  -> runStmt s

  DoWhileStatement loc e s -> -- ref 12.6.1
    runStmt $ transformDoWhileToWhile loc e s

  WhileStatement loc e stmt -> keepGoing Nothing where -- ref 12.6.2
    keepGoing :: Maybe JSVal -> Runtime StmtReturn
    keepGoing v = do
      willEval <- isTruthy <$> (runExprStmt cxt e >>= getValue)

      if not willEval
      then return (CTNormal, v, Nothing)
      else do
        sval@(stype, v', _) <- runStmt stmt
        let nextVal = case v' of
                        Nothing -> v
                        Just newVal -> Just newVal
        case stype of
          CTBreak -> return (CTNormal, v, Nothing)
          CTContinue -> keepGoing nextVal
          CTNormal -> keepGoing nextVal
          _ -> return sval

  For loc (For3 e1 e2 e3) stmt -> -- ref 12.6.3
    runStmt $ transformFor3ToWhile loc e1 e2 e3 stmt

  For loc (For3Var x e1 e2 e3) stmt -> -- ref 12.6.3
    runStmt $ transformFor3VarToWhile loc x e1 e2 e3 stmt

  Block loc stmts -> runStmts stmts

  TryStatement _loc block catch finally -> do -- ref 12.14
    b@(btype, bval, _) <- runStmt block
    if btype /= CTThrow
    then return b
    else runCatch cxt catch (fromJust bval)

  ThrowStatement loc e -> do
    exc <- runExprStmt cxt e >>= getValue
    return (CTThrow, Just exc, Nothing)

  BreakStatement loc label -> return (CTBreak, Nothing, label)
  ContinueStatement loc label -> return (CTBreak, Nothing, label)
  Return loc Nothing -> return (CTReturn, Just VUndef, Nothing)
  Return loc (Just e) -> do
    val <- runExprStmt cxt e >>= getValue
    return (CTReturn, Just val, Nothing)

  EmptyStatement loc -> return (CTNormal, Nothing, Nothing)

  _ -> error ("Unimplemented stmt: " ++ show s) }

-- |
-- Turn "for (e1; e2; e3) { s }" into
-- "e1; while (e2) { s; e3 }"
-- with sensible defaults for missing statements
transformFor3ToWhile :: SrcLoc -> Maybe Expr -> Maybe Expr -> Maybe Expr -> Statement -> Statement
transformFor3ToWhile loc e1 e2 e3 stmt =
  let e = fromMaybe (Boolean True) e2
      s1 = maybe (EmptyStatement loc) (ExprStmt loc) e1
      s2 = [ stmt, maybe (EmptyStatement loc) (ExprStmt loc) e3 ]
  in Block loc [ s1, WhileStatement loc e (Block loc s2) ]

-- |
-- Turn "for (var x = e1; e2; e3) { s }" into
-- "var x = e1; while (e2) { s; e3 }"
-- with sensible defaults for missing statements
transformFor3VarToWhile :: SrcLoc -> Ident -> Expr -> Maybe Expr -> Maybe Expr -> Statement -> Statement
transformFor3VarToWhile loc x e1 e2 e3 stmt =
  let e = fromMaybe (Boolean True) e2
      s1 = VarDecl loc [(x, Just e1)]
      s2 = [ stmt, maybe (EmptyStatement loc) (ExprStmt loc) e3 ]
  in Block loc [ s1, WhileStatement loc e (Block loc s2) ]

-- |
-- Turn "do { s } while (e)" into
-- "while (true) { s; if (!e) break; }"
transformDoWhileToWhile :: SrcLoc -> Expr -> Statement -> Statement
transformDoWhileToWhile loc e s =
  let esc = IfStatement loc (UnOp "!" e)
              (BreakStatement loc Nothing)
              Nothing
  in WhileStatement loc (Boolean True) $ Block loc [ s, esc ]


-- ref 12.14
runCatch :: JSCxt -> Maybe Catch -> JSVal -> Runtime StmtReturn
runCatch _ Nothing _ = return (CTNormal, Nothing, Nothing)
runCatch cxt (Just (Catch _loc var stmt)) exc = do
  -- XXX create new environment
  putVar cxt var exc
  runStmt stmt

maybeValList :: JSCxt -> [Maybe Expr] -> Runtime [Maybe JSVal]
maybeValList cxt = mapM (evalOne cxt)
  where
    evalOne cxt v = case v of
      Nothing -> return Nothing
      Just e  -> Just <$> (runExprStmt cxt e >>= getValue)

readVar :: JSCxt -> Ident -> Runtime JSVal
readVar cxt name = do
  strictness <- return . cxtStrictness =<< getGlobalContext
  VRef <$> getIdentifierReference (Just $ lexEnv cxt) name strictness

runExprStmt :: JSCxt -> Expr -> Runtime JSVal
runExprStmt cxt expr = case expr of
  Num n              -> return $ VNum n
  Str s              -> return $ VStr s
  Boolean b          -> return $ VBool b
  LiteralNull        -> return VNull
  ReadVar name       -> readVar cxt name
  This               -> return $ thisBinding cxt
  ArrayLiteral vals  -> evalArrayLiteral cxt vals
  MemberDot e x      -> runExprStmt cxt (MemberGet e (Str x)) -- ref 11.2.1

  MemberGet e x -> do -- ref 11.2.1
    lval <- runExprStmt cxt e >>= getValue
    prop <- runExprStmt cxt x >>= getValue >>= toString
    memberGet lval prop

  FunCall f args -> do  -- ref 11.2.3
    ref <- runExprStmt cxt f
    argList <- evalArguments cxt args
    funCall cxt ref argList

  Assign lhs op e -> do
    lref <- runExprStmt cxt lhs
    rref <- runExprStmt cxt e
    case op of
      "=" -> assignRef lref rref
      _   -> updateRef (init op) lref rref

  BinOp "&&" e1 e2 -> do
    v1 <- runExprStmt cxt e1 >>= getValue
    if toBoolean v1
    then runExprStmt cxt e2 >>= getValue
    else return v1

  BinOp "||" e1 e2 -> do
    v1 <- runExprStmt cxt e1 >>= getValue
    if toBoolean v1
    then return v1
    else runExprStmt cxt e2 >>= getValue

  BinOp op e1 e2 -> do
    v1 <- runExprStmt cxt e1 >>= getValue
    v2 <- runExprStmt cxt e2 >>= getValue
    evalBinOp op v1 v2

  UnOp "typeof" e -> do -- ref 11.4.3
    evalTypeof =<< runExprStmt cxt e

  UnOp op e -> -- ref 11.4
    let f = case op of
              "++"   -> modifyingOp (+ 1) (+ 1)
              "--"   -> modifyingOp (subtract 1) (subtract 1)
              "+"    -> purePrefix unaryPlus
              "-"    -> purePrefix unaryMinus
              "!"    -> purePrefix unaryNot
              "~"    -> purePrefix unaryBitwiseNot
              "void" -> purePrefix (return . const VUndef)
              _    -> const $ const $ raiseError $ "Prefix not implemented: " ++ op
    in f cxt e

  PostOp op e -> -- ref 11.3
    let f = case op of
              "++" -> modifyingOp (+1) id
              "--" -> modifyingOp (subtract 1) id
    in f cxt e

  NewExpr f args -> do
    fun <- runExprStmt cxt f
    argList <- evalArguments cxt args
    liftM VObj (newObjectFromConstructor cxt fun argList)

  FunDef (Just name) params strictness body -> do
    fun <- createFunction params strictness body cxt
    putVar cxt name fun
    return fun

  FunDef Nothing params strictness body -> createFunction params strictness body cxt

  ObjectLiteral nameValueList -> makeObjectLiteral cxt nameValueList

  _              -> error ("Unimplemented expr: " ++ show expr)

evalArrayLiteral :: JSCxt -> [Maybe Expr] -> Runtime JSVal
evalArrayLiteral cxt vals = createArray =<< mapM evalMaybe vals
  where evalMaybe Nothing = return Nothing
        evalMaybe (Just e) = Just <$> (runExprStmt cxt e >>= getValue)

evalArguments :: JSCxt -> [Expr] -> Runtime [JSVal]
evalArguments cxt = mapM (runExprStmt cxt >=> getValue)

modifyingOp :: (JSNum->JSNum) -> (JSNum->JSNum) -> JSCxt -> Expr -> Runtime JSVal
modifyingOp op returnOp cxt e = do
  lhs <- runExprStmt cxt e
  case lhs of
    VRef ref -> do
      lval <- getValue lhs
      val <- toNumber lval
      let newVal = VNum $ op val
          retVal = VNum $ returnOp val
      putValue ref newVal
      return retVal
    _ -> raiseReferenceError $ show e ++ " is not assignable"

purePrefix :: (JSVal -> Runtime JSVal) -> JSCxt -> Expr -> Runtime JSVal
purePrefix f cxt e = runExprStmt cxt e >>= getValue >>= f



makeObjectLiteral :: JSCxt -> [(PropertyName, Expr)] -> Runtime JSVal
makeObjectLiteral cxt nameValueList =
  let nameOf :: PropertyName -> String
      nameOf (IdentProp ident) = ident
      nameOf (StringProp str)  = str
      nameOf (NumProp n)       = show n
      addProp :: Shared JSObj -> (PropertyName, Expr) -> Runtime (Shared JSObj)
      addProp obj (propName, propValue) = do
        val <- runExprStmt cxt propValue >>= getValue
        addOwnProperty (nameOf propName) val obj
  in do
    obj <- newObject
    VObj <$> foldM addProp obj nameValueList


evalTypeof :: JSVal -> Runtime JSVal
evalTypeof val = do
  if isReference val && isUnresolvableReference (unwrapRef val)
  then return $ VStr "undefined"
  else do
    resolved <- getValue val
    return $ VStr $ case typeof resolved of
      TypeUndefined -> "undefined"
      TypeNull      -> "null"
      TypeBoolean   -> "boolean"
      TypeNumber    -> "number"
      TypeString    -> "string"
      TypeObject    -> "object"  -- or "function"
      _ -> showVal resolved
