{-# LANGUAGE LambdaCase #-}

module Eval (runJS, evalJS, jsEvalExpr, runtime, runtime', RuntimeError(..)) where

import Control.Monad.State
import Control.Monad.Except
import Control.Monad.Writer
import Control.Applicative
import Data.Maybe
import Data.List (intercalate)
import Data.Bits
import Data.Foldable
import Text.Show.Functions
import Parse
import Expr
import JSNum
import Runtime

import Debug.Trace

data RuntimeError = RuntimeError {
  errorMessage :: String,
  errorObject :: JSVal,
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
    (runExprStmt (parseExpr input) >>= getValue) `catchError` rethrowAsString
  case result of
    Left err  -> do
      error (show err)
    Right val -> return val

rethrowAsString :: JSError -> Runtime a
rethrowAsString (JSError (err, s)) = do
  v <- callToString err
  throwError $ JSError (v, s)

toRuntimeError :: JSError -> RuntimeError
toRuntimeError (JSError (VStr err, stack)) = RuntimeError err (VStr err) (map show stack)
toRuntimeError _ = error "Runtime did not convert error to string"

evalCode :: String -> Runtime StmtReturn
evalCode text = do
  currentStrictness <- return . cxtStrictness =<< getGlobalContext
  case parseJS'' text "(eval)" currentStrictness False of
    Left err -> raiseSyntaxError (show err)
    Right (Program strictness stmts) ->
      withStrictness strictness (runStmts stmts)

runJS' :: String -> String -> IO ((Either JSError (Maybe JSVal), String), JSGlobal)
runJS' sourceName input = case parseJS' input sourceName of
  Left err -> return ((Left $ JSError (VStr $ "SyntaxError: " ++ show err, []), ""), emptyGlobal)
  Right ast -> runRuntime (runProg ast)

runtime :: Runtime a -> IO (Either JSError a)
runtime p = do
  result <- runRuntime p
  return $ fst (fst result)

runtime' :: Runtime a -> IO (Either JSError a)
runtime' p = runtime (initGlobals >> p)

initGlobals :: Runtime ()
initGlobals = do
  objProto <- createGlobalObjectPrototype
  modify $ \st -> st { globalObjectPrototype = Just objProto }

  newGlobalObject <- createGlobalThis
  modify $ \st -> st { globalObject = Just newGlobalObject,
                       globalEvaluator = Just evalCode,
                       globalRun = Just runStmts }
  cxt <- initialCxt
  modify $ \st -> st { globalContext = Just cxt }


runProg :: Program -> Runtime (Maybe JSVal)
runProg (Program strictness stmts) = do
  initGlobals
  result <- withStrictness strictness (runStmts stmts)
  case result of
    (CTNormal, v, _) -> return v
    (CTThrow, Just v, _) -> do
      msg <- callToString v
      st <- getStackTrace v
      throwError $ JSError (msg, st)
    _ -> do
      liftIO $ putStrLn $ "Abnormal exit: " ++ show result
      return Nothing

callToString :: JSVal -> Runtime JSVal
callToString (VStr str) = return (VStr str)
callToString v = do
  obj <- toObject v
  ref <- memberGet (VObj obj) "toString"
  funCall ref []

getStackTrace :: JSVal -> Runtime [SrcLoc]
getStackTrace (VObj obj) = do
  st <- objGet "stack" obj
  return $ case st of
    VStacktrace s -> s
    _ -> []
getStackTrace _ = return []

returnThrow :: Statement -> JSError -> Runtime StmtReturn
returnThrow s (JSError (err, stack)) = do
  setStacktrace err (sourceLocation s : stack)
  return (CTThrow, Just err, Nothing)
returnThrow s (JSProtoError (t, msg)) = do
  err <- createError t (VStr msg)
  returnThrow s (JSError (err, []))


setStacktrace :: JSVal -> [SrcLoc] -> Runtime ()
setStacktrace v stack =
  case v of
    VObj objRef -> void $ addOwnProperty "stack" (VStacktrace stack) objRef
    _           -> return ()

addToStacktrace :: SrcLoc -> JSVal -> Runtime ()
addToStacktrace loc (VObj objRef) = do
  val <- objGet "stack" objRef
  case val of
    VStacktrace st -> objPut "stack" (VStacktrace $ loc : st) False objRef
    _ -> return ()
addToStacktrace loc val = return ()


-- ref 12.1
runStmts :: [Statement] -> Runtime StmtReturn
runStmts = runStmts' (CTNormal, Nothing, Nothing) where
  runStmts' emptyResult [] = return emptyResult
  runStmts' _ (s:stmts) = do
    result <- runStmt s `catchError` returnThrow s
    case result of
      (CTThrow, Just v, _) -> addToStacktrace (sourceLocation s) v >> return result
      (CTNormal, _, _) -> runStmts' result stmts
      _ -> return result

runStmt :: Statement -> Runtime StmtReturn
runStmt s = case s of
  ExprStmt _loc e -> do
    val <- runExprStmt e >>= getValue
    return (CTNormal, Just val, Nothing)

  VarDecl _loc assignments -> do
    forM_ assignments $ \(x, e) -> case e of
      Nothing  -> putVar x VUndef
      Just e' -> do
        v <- getValue =<< runExprStmt e'
        when (x == "eval" || x == "arguments") $
          cannotAssignTo x
        putVar x v
    return (CTNormal, Nothing, Nothing)

  LabelledStatement _loc _label stmt -> runStmt stmt

  IfStatement _loc predicate ifThen ifElse -> do -- ref 12.5
    v <- runExprStmt predicate >>= getValue
    if toBoolean v
    then runStmt ifThen
    else case ifElse of
           Nothing -> return (CTNormal, Nothing, Nothing)
           Just stmt  -> runStmt stmt

  DoWhileStatement _loc e stmt -> -- ref 12.6.1
    runStmt $ transformDoWhileToWhile _loc e stmt

  WhileStatement _loc e stmt -> keepGoing Nothing where -- ref 12.6.2
    keepGoing :: Maybe JSVal -> Runtime StmtReturn
    keepGoing v = do
      willEval <- toBoolean <$> (runExprStmt e >>= getValue)

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

  For _loc (ForIn lhs e) stmt -> do -- ref 12.6.4
    exprRef <- runExprStmt e
    exprValue <- getValue exprRef
    if (exprValue == VNull || exprValue == VUndef)
    then return (CTNormal, Nothing, Nothing)
    else do
      obj <- toObject exprValue
      return (CTNormal, Just (VObj obj), Nothing)

  For loc (ForInVar (x, e1) e2) stmt -> do
    runStmt $ transformFor3VarIn loc x e1 e2 stmt

  Block _loc stmts -> runStmts stmts

  TryStatement _loc block catch _finally -> do -- ref 12.14  XXX ignoring finally block
    b@(btype, bval, _) <- runStmt block
    if btype /= CTThrow
    then return b
    else runCatch catch (fromJust bval)

  ThrowStatement _loc e -> do
    exc <- runExprStmt e >>= getValue
    return (CTThrow, Just exc, Nothing)

  BreakStatement _loc label -> return (CTBreak, Nothing, label)
  ContinueStatement _loc label -> return (CTBreak, Nothing, label)
  Return _loc Nothing -> return (CTReturn, Just VUndef, Nothing)
  Return _loc (Just e) -> do
    val <- runExprStmt e >>= getValue
    return (CTReturn, Just val, Nothing)

  EmptyStatement _loc -> return (CTNormal, Nothing, Nothing)

  _ -> error ("Unimplemented stmt: " ++ show s)

-- |
-- Turn "for (e1; e2; e3) { s }" into
-- "e1; while (e2) { s; e3 }"
-- with sensible defaults for missing statements
transformFor3ToWhile :: SrcLoc -> Maybe Expr -> Maybe Expr -> Maybe Expr -> Statement -> Statement
transformFor3ToWhile _loc e1 e2 e3 stmt =
  let e = fromMaybe (Boolean True) e2
      s1 = maybe (EmptyStatement _loc) (ExprStmt _loc) e1
      s2 = [ stmt, maybe (EmptyStatement _loc) (ExprStmt _loc) e3 ]
  in Block _loc [ s1, WhileStatement _loc e (Block _loc s2) ]

-- |
-- Turn "for (var x = e1; e2; e3) { s }" into
-- "var x = e1; while (e2) { s; e3 }"
-- with sensible defaults for missing statements
transformFor3VarToWhile :: SrcLoc -> Ident -> Expr -> Maybe Expr -> Maybe Expr -> Statement -> Statement
transformFor3VarToWhile _loc x e1 e2 e3 stmt =
  let e = fromMaybe (Boolean True) e2
      s1 = VarDecl _loc [(x, Just e1)]
      s2 = [ stmt, maybe (EmptyStatement _loc) (ExprStmt _loc) e3 ]
  in Block _loc [ s1, WhileStatement _loc e (Block _loc s2) ]

-- |
-- Turn "do { s } while (e)" into
-- "while (true) { s; if (!e) break; }"
transformDoWhileToWhile :: SrcLoc -> Expr -> Statement -> Statement
transformDoWhileToWhile loc e s =
  let esc = IfStatement loc (UnOp "!" e)
              (BreakStatement loc Nothing)
              Nothing
  in WhileStatement loc (Boolean True) $ Block loc [ s, esc ]

transformFor3VarIn :: SrcLoc -> Ident -> Maybe Expr -> Expr -> Statement -> Statement
transformFor3VarIn loc x e1 e2 s =
  let s1 = VarDecl loc [(x, e1)]
  in Block loc [ s1, For loc (ForIn (ReadVar x) e2) s ]

-- ref 12.14
runCatch :: Maybe Catch -> JSVal -> Runtime StmtReturn
runCatch Nothing _ = return (CTNormal, Nothing, Nothing)
runCatch (Just (Catch _loc var stmt)) exc = do
  -- XXX create new environment
  putVar var exc
  runStmt stmt

maybeValList :: [Maybe Expr] -> Runtime [Maybe JSVal]
maybeValList = mapM evalOne
  where
    evalOne v = case v of
      Nothing -> return Nothing
      Just e  -> Just <$> (runExprStmt e >>= getValue)

readVar :: Ident -> Runtime JSVal
readVar name = do
  cxt <- getGlobalContext
  VRef <$> getIdentifierReference (Just $ lexEnv cxt) name (cxtStrictness cxt)

runExprStmt :: Expr -> Runtime JSVal
runExprStmt expr = case expr of
  Num n              -> return $ VNum n
  Str s              -> return $ VStr s
  Boolean b          -> return $ VBool b
  LiteralNull        -> return VNull
  ReadVar name       -> readVar name
  This               -> thisBinding <$> getGlobalContext
  ArrayLiteral vals  -> evalArrayLiteral vals
  MemberDot e x      -> runExprStmt (MemberGet e (Str x)) -- ref 11.2.1

  MemberGet e x -> do -- ref 11.2.1
    baseValue <- runExprStmt e >>= getValue
    propertyNameValue <- runExprStmt x >>= getValue >>= toString
    checkObjectCoercible(baseValue)
    memberGet baseValue propertyNameValue

  FunCall f args -> do  -- ref 11.2.3
    ref <- runExprStmt f
    argList <- evalArguments args
    funCall ref argList

  Assign lhs op e -> do
    lref <- runExprStmt lhs
    rref <- runExprStmt e
    case op of
      "=" -> assignRef lref rref
      _   -> updateRef (init op) lref rref

  BinOp "&&" e1 e2 -> do
    v1 <- runExprStmt e1 >>= getValue
    if toBoolean v1
    then runExprStmt e2 >>= getValue
    else return v1

  BinOp "||" e1 e2 -> do
    v1 <- runExprStmt e1 >>= getValue
    if toBoolean v1
    then return v1
    else runExprStmt e2 >>= getValue

  BinOp op e1 e2 -> do
    v1 <- runExprStmt e1 >>= getValue
    v2 <- runExprStmt e2 >>= getValue
    evalBinOp op v1 v2

  UnOp "delete" e -> do -- ref 11.4.1
    evalDelete =<< runExprStmt e
  UnOp "typeof" e -> do -- ref 11.4.3
    evalTypeof =<< runExprStmt e
  UnOp op e -> -- ref 11.4
    let f = case op of
              "++"   -> modifyingOp (+ 1) (+ 1)
              "--"   -> modifyingOp (subtract 1) (subtract 1)
              "+"    -> purePrefix unaryPlus
              "-"    -> purePrefix unaryMinus
              "!"    -> purePrefix unaryNot
              "~"    -> purePrefix unaryBitwiseNot
              "void" -> purePrefix (return . const VUndef)
              _    -> const $ raiseError $ "Prefix not implemented: " ++ op
    in f e

  PostOp op e -> -- ref 11.3
    let f = case op of
              "++" -> modifyingOp (+1) id
              "--" -> modifyingOp (subtract 1) id
              _    -> const $ raiseError $ "No such postfix operator: " ++ op
    in f e

  NewExpr f args -> do
    fun <- runExprStmt f
    argList <- evalArguments args
    liftM VObj (newObjectFromConstructor fun argList)

  FunDef (Just name) params strictness body -> do
    fun <- createFunction params strictness body
    putVar name fun
    return fun

  FunDef Nothing params strictness body -> createFunction params strictness body

  ObjectLiteral nameValueList -> makeObjectLiteral nameValueList

  _              -> error ("Unimplemented expr: " ++ show expr)

evalArrayLiteral :: [Maybe Expr] -> Runtime JSVal
evalArrayLiteral vals = createArray =<< mapM evalMaybe vals
  where evalMaybe Nothing = return Nothing
        evalMaybe (Just e) = Just <$> (runExprStmt e >>= getValue)

evalArguments :: [Expr] -> Runtime [JSVal]
evalArguments = mapM (runExprStmt >=> getValue)

modifyingOp :: (JSNum->JSNum) -> (JSNum->JSNum) -> Expr -> Runtime JSVal
modifyingOp op returnOp e = do
  lhs <- runExprStmt e
  case lhs of
    VRef ref -> do
      lval <- getValue lhs
      val <- toNumber lval
      let newVal = VNum $ op val
          retVal = VNum $ returnOp val
      putValue ref newVal
      return retVal
    _ -> raiseReferenceError $ show e ++ " is not assignable"

purePrefix :: (JSVal -> Runtime JSVal) -> Expr -> Runtime JSVal
purePrefix f e = runExprStmt e >>= getValue >>= f



makeObjectLiteral :: [(PropertyName, Expr)] -> Runtime JSVal
makeObjectLiteral nameValueList =
  let nameOf :: PropertyName -> String
      nameOf (IdentProp ident) = ident
      nameOf (StringProp str)  = str
      nameOf (NumProp n)       = show n
      addProp :: Shared JSObj -> (PropertyName, Expr) -> Runtime (Shared JSObj)
      addProp obj (p, v) = do
        val <- runExprStmt v >>= getValue
        addOwnProperty (nameOf p) val obj
  in do
    obj <- newObject
    VObj <$> foldM addProp obj nameValueList

-- ref 11.4.1
evalDelete :: JSVal -> Runtime JSVal
evalDelete val
  | not (isReference val) = return (VBool True)
  | isUnresolvableReference ref && isStrictReference ref = raiseSyntaxError "Delete of an unqualified identifier in strict mode (1)"
  | isUnresolvableReference ref = return (VBool True)
  | isPropertyReference ref = deleteFromObj ref
  | isStrictReference ref = raiseSyntaxError "Delete of an unqualified identifier in strict mode (2)"
  | otherwise = deleteFromEnv ref

  where ref = unwrapRef val
        deleteFromObj (JSRef base name strict) = do
          obj <- toObject base
          objDelete name (strict == Strict) obj
        deleteFromEnv (JSRef (VEnv base) name strict) = deleteBinding name base


-- ref 11.4.3
evalTypeof :: JSVal -> Runtime JSVal
evalTypeof val = do
  if isReference val && isUnresolvableReference (unwrapRef val)
  then return $ VStr "undefined"
  else do
    resolved <- getValue val
    result <- case resolved of
      VObj objRef -> do
        cls <- objClass <$> deref objRef
        return $ if cls == "Function"
        then "function"
        else "object"
      _ ->
        return $ case typeof resolved of
          TypeUndefined -> "undefined"
          TypeNull      -> "object"
          TypeBoolean   -> "boolean"
          TypeNumber    -> "number"
          TypeString    -> "string"
          _ -> showVal resolved
    return $ VStr result
