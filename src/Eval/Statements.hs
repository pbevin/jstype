{-# LANGUAGE LambdaCase #-}

module Eval.Statements where

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
import Parse
import Expr
import JSNum
import Runtime
import Builtins
import Runtime
import Core


runProg :: Program -> Runtime (Maybe JSVal)
runProg (Program strictness stmts) = do
  result <- withStrictness strictness (runStmts stmts)
  case result of
    CTNormal v       -> return v
    CTThrow (Just v) -> throwValAsException v
    otherwise        -> do
      liftIO $ putStrLn $ "Abnormal exit: " ++ show result
      return Nothing

  where
    throwValAsException :: JSVal -> Runtime a
    throwValAsException v = do
      msg <- toString v
      st <- getStackTrace v
      throwError $ JSError (VStr msg, st)

-- ref 12.1
runStmts :: [Statement] -> Runtime StmtReturn
runStmts = runStmt . desugar


runStmt :: CoreStatement -> Runtime StmtReturn
runStmt stmt = action `catchError` returnThrow stmt
  where
    action = case stmt of
      CoreBind dbiType bindings stmt -> runCoreBinding dbiType bindings stmt
      CoreBlock stmts                -> runCoreBlock stmts
      CoreExpr _loc e                -> runCoreExpr e
      CoreIf _loc e ifThen ifElse    -> runCoreIf e ifThen ifElse
      CoreLoop _loc test inc body    -> runCoreLoop test inc body
      CoreBreak _loc label           -> runCoreBreak label
      CoreCont _loc label            -> runCoreCont label
      CoreCase _loc e cases          -> runCoreCase e cases
      CoreLabel _loc label body      -> runCoreLabel label body
      Unconverted s                  -> runUnconvertedStmt s
      otherwise                      -> error $ "Can't execute " ++ show otherwise

    returnThrow :: CoreStatement -> JSError -> Runtime StmtReturn
    returnThrow s (JSError (err, stack)) = do
      setStacktrace err (sourceLocation s : stack)
      return $ CTThrow (Just err)
    returnThrow s (JSProtoError (t, msg)) = do
      err <- createError t (VStr msg)
      returnThrow s (JSError (err, []))


runCoreBinding :: DBIType -> [(Ident, Expr)] -> CoreStatement -> Runtime StmtReturn
runCoreBinding dbiType bindings stmt = do
  bindAll dbiType NotStrict bindings
  runStmt stmt

runCoreBlock :: [CoreStatement] -> Runtime StmtReturn
runCoreBlock stmts = runAll (CTNormal Nothing) stmts
  where
    runAll :: StmtReturn -> [CoreStatement] -> Runtime StmtReturn
    runAll r [] = return r
    runAll _ (s:stmts) = do
      result <- runStmt s
      case result of
        CTNormal _ -> runAll result stmts
        otherwise  -> return result

runCoreExpr :: Expr -> Runtime StmtReturn
runCoreExpr e = CTNormal . Just <$> (runExprStmt e >>= getValue)

runCoreIf :: Expr -> CoreStatement -> Maybe CoreStatement -> Runtime StmtReturn
runCoreIf e ifThen ifElse = do
  cond <- toBoolean <$> (runExprStmt e >>= getValue)
  case (cond, ifElse) of
    (True, _)        -> runStmt ifThen
    (False, Just s)  -> runStmt s
    (False, Nothing) -> return (CTNormal Nothing)

runCoreLoop :: Expr -> Expr -> CoreStatement -> Runtime StmtReturn
runCoreLoop test inc body = keepGoing Nothing where
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
        CTBreak    v' _ -> return $ CTNormal v'
        CTContinue v' _ -> increment >> keepGoing v'
        CTNormal   v'   -> increment >> keepGoing v'
        otherwise      -> return r

runCoreBreak :: Maybe Label -> Runtime StmtReturn
runCoreBreak = return . CTBreak Nothing

runCoreCont :: Maybe Label -> Runtime StmtReturn
runCoreCont = return . CTContinue Nothing

runCoreCase :: Expr -> [(Maybe Expr, CoreStatement)] -> Runtime StmtReturn
runCoreCase e cases =
  runExprStmt e >>= getValue >>= go cases
    where
      go :: [(Maybe Expr, CoreStatement)] -> JSVal -> Runtime StmtReturn
      go cs input = case cs of
        []       -> endWith dflt
        (c:rest) -> case c of
          (Nothing, s) -> go rest input
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
          otherwise   -> return $ r { rval = v' }

      dflt = map snd . dropWhile (isJust . fst) $ cases

runCoreLabel :: Label -> CoreStatement -> Runtime StmtReturn
runCoreLabel _ = runStmt



setStacktrace :: JSVal -> [SrcLoc] -> Runtime ()
setStacktrace v stack =
  case v of
    VObj objRef -> void $ addOwnProperty "stack" (VStacktrace stack) objRef
    _           -> return ()

runUnconvertedStmt :: Statement -> Runtime StmtReturn
runUnconvertedStmt s = {-# SCC stmt #-} case s of
  FunDecl {} -> return (CTNormal Nothing)
  VarDecl _loc assignments -> runVarDecl assignments -- ref 12.2

  For _loc (ForIn lhs e) stmt -> do -- ref 12.6.4
    exprRef <- runExprStmt e
    exprValue <- getValue exprRef
    if (exprValue == VNull || exprValue == VUndef)
    then return $ CTNormal Nothing
    else do
      obj <- toObject exprValue
      keys <- propMapKeys . ownProperties <$> deref obj
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
            s <- runStmt (convert stmt)
            let nextv = rval s <|> v
            case s of
              CTBreak v' _    -> return $ CTNormal nextv
              CTContinue v' _ -> keepGoing obj ps  nextv
              CTNormal _      -> keepGoing obj ps  nextv
              _ -> return s
          else keepGoing obj ps v


  For loc (ForInVar (x, e1) e2) stmt ->
    runStmt . convert $ transformFor3VarIn loc x e1 e2 stmt

  WithStatement _loc e s -> runWithStatement e s

  TryStatement _loc block catch finally -> do -- ref 12.14
    br <- runStmt (convert block)

    case (catch, finally) of
      (Just c, Nothing) -> do
        case br of
          CTThrow (Just exc) -> runCatch c exc
          otherwise          -> return br

      (Nothing, Just f) -> do
        fr <- runFinally f
        case fr of
          CTNormal _ -> return br
          otherwise  -> return fr

      (Just c, Just f) -> do
        cr <- case br of
                CTThrow (Just exc) -> runCatch c exc
                otherwise          -> return br
        fr <- runFinally f
        case fr of
          CTNormal _ -> return cr
          otherwise  -> return fr

  ThrowStatement _loc e -> do
    exc <- runExprStmt e >>= getValue
    return $ CTThrow (Just exc)

  Return _loc Nothing          -> return $ CTReturn  (Just VUndef)
  Return _loc (Just e) -> do
    val <- runExprStmt e >>= getValue
    return $ CTReturn (Just val)

  EmptyStatement _loc -> return $ CTNormal Nothing

  _ -> error ("Unimplemented stmt: " ++ show s)


transformFor3VarIn :: SrcLoc -> Ident -> Maybe Expr -> Expr -> Statement -> Statement
transformFor3VarIn loc x e1 e2 s =
  let s1 = VarDecl loc [(x, e1)]
  in Block loc [ s1, For loc (ForIn (ReadVar x) e2) s ]

-- ref 12.2
runVarDecl :: [VarDeclaration] -> Runtime StmtReturn
runVarDecl assignments = do
  forM_ assignments $ \(x, e) -> case e of
    Nothing  -> return ()
    Just e' -> do
      runExprStmt e' >>= getValue >>= putVar x
  return $ CTNormal Nothing

-- ref 12.10
runWithStatement :: Expr -> Statement -> Runtime StmtReturn
runWithStatement e s = do
  val <- runExprStmt e
  obj <- toObject =<< getValue val
  oldEnv <- lexEnv <$> getGlobalContext
  newEnv <- newObjectEnvironment obj (Just oldEnv) True
  withLexEnv newEnv $ runStmt (convert s)



-- ref 12.14
runCatch :: Statement -> JSVal -> Runtime StmtReturn
runCatch (Catch _loc var block) c = do
  oldEnv <- lexEnv <$> getGlobalContext
  catchEnv <- newDeclarativeEnvironment (Just oldEnv)
  rec <- envRec <$> deref catchEnv
  createMutableBinding var True rec
  setMutableBinding var c False rec
  withLexEnv catchEnv $ runStmt (convert block)

runFinally :: Statement -> Runtime StmtReturn
runFinally (Finally _loc stmt) = runStmt (convert stmt)

readVar :: Ident -> Runtime JSVal
readVar name = do
  cxt <- getGlobalContext
  VRef <$> getIdentifierReference (Just $ lexEnv cxt) name (cxtStrictness cxt)

runExprStmt :: Expr -> Runtime JSVal
runExprStmt expr = case expr of
  Num n                 -> return $ VNum n
  Str s                 -> return $ VStr s
  Boolean b             -> return $ VBool b
  LiteralNull           -> return VNull
  LiteralUndefined      -> return VUndef
  ReadVar name          -> readVar name
  This                  -> thisBinding <$> getGlobalContext
  ArrayLiteral vals     -> evalArrayLiteral vals
  ObjectLiteral map     -> makeObjectLiteral map
  RegularExpression r f -> makeRegularExpression r f
  MemberDot e x         -> evalPropertyAccessorIdent e x
  MemberGet e x         -> evalPropertyAccessor e x
  FunCall f args        -> evalFunCall f args
  Assign lhs op e       -> evalAssignment lhs op e
  Cond e1 e2 e3         -> evalCond e1 e2 e3
  BinOp "&&" e1 e2      -> evalAndAnd e1 e2
  BinOp "||" e1 e2      -> evalOrOr e1 e2
  BinOp op e1 e2        -> evalBinaryOp op e1 e2
  UnOp "delete" e       -> runExprStmt e >>= evalDelete -- ref 11.4.1
  UnOp "typeof" e       -> runExprStmt e >>= evalTypeof -- ref 11.4.3
  UnOp op e             -> evalUnOp op e
  PostOp op e           -> evalPostOp op e
  NewExpr f args        -> evalNewExpr f args
  FunExpr n ps st body  -> evalFunExpr n ps st body

-- ref 11.2.1
evalPropertyAccessor :: Expr -> Expr -> Runtime JSVal
evalPropertyAccessor e x = do
  baseValue <- runExprStmt e >>= getValue
  propertyNameValue <- runExprStmt x >>= getValue
  checkObjectCoercible ("Cannot read property " ++ showVal (propertyNameValue)) baseValue
  propertyNameString <- toString propertyNameValue
  memberGet baseValue propertyNameString

-- ref 11.2.1
evalPropertyAccessorIdent :: Expr -> Ident -> Runtime JSVal
evalPropertyAccessorIdent e x = do
  baseValue <- runExprStmt e >>= getValue
  checkObjectCoercible ("Cannot read property " ++ x) baseValue
  memberGet baseValue x


-- ref 11.2.2
evalNewExpr :: Expr -> [Expr] -> Runtime JSVal
evalNewExpr f args = do
  fun <- runExprStmt f >>= getValue
  argList <- evalArguments args
  assertFunction (show f) cstrMethod fun  -- XXX need to get the name here
  liftM VObj (newObjectFromConstructor fun argList)

-- ref 11.13.1 (simple assignment)
-- ref 11.13.2 (compound assignment)
evalAssignment :: Expr -> Ident -> Expr -> Runtime JSVal
evalAssignment lhs op e = do
  lref <- runExprStmt lhs
  rref <- runExprStmt e
  case op of
    "=" -> assignRef lref rref
    _   -> updateRef (init op) lref rref

evalCond :: Expr -> Expr -> Expr -> Runtime JSVal
evalCond e1 e2 e3 = do
  lref <- runExprStmt e1 >>= getValue
  if toBoolean lref
  then runExprStmt e2 >>= getValue
  else runExprStmt e3 >>= getValue

evalAndAnd :: Expr -> Expr -> Runtime JSVal
evalAndAnd e1 e2 = do
  v1 <- runExprStmt e1 >>= getValue
  if toBoolean v1
  then runExprStmt e2 >>= getValue
  else return v1

evalOrOr :: Expr -> Expr -> Runtime JSVal
evalOrOr e1 e2 = do
  v1 <- runExprStmt e1 >>= getValue
  if toBoolean v1
  then return v1
  else runExprStmt e2 >>= getValue

evalBinaryOp :: Ident -> Expr -> Expr -> Runtime JSVal
evalBinaryOp op e1 e2 = do
  v1 <- runExprStmt e1 >>= getValue
  v2 <- runExprStmt e2 >>= getValue
  evalBinOp op v1 v2

evalUnOp :: Ident -> Expr -> Runtime JSVal
evalUnOp op e = f e
  where 
    f = case op of
          "++"   -> modifyingOp (+ 1) (+ 1)
          "--"   -> modifyingOp (subtract 1) (subtract 1)
          "+"    -> purePrefix unaryPlus
          "-"    -> purePrefix unaryMinus
          "!"    -> purePrefix unaryNot
          "~"    -> purePrefix unaryBitwiseNot
          "void" -> purePrefix (return . const VUndef)
          _      -> const $ raiseError $ "Prefix not implemented: " ++ op

evalFunExpr :: Maybe Ident -> [Ident] -> Strictness -> [Statement] -> Runtime JSVal
evalFunExpr name params strictness body = do
  env <- lexEnv <$> getGlobalContext
  createFunction name params strictness body env

-- ref 11.3
evalPostOp :: Ident -> Expr -> Runtime JSVal
evalPostOp op e = f e
  where f = case op of
          "++" -> modifyingOp (+1) id
          "--" -> modifyingOp (subtract 1) id
          _    -> const $ raiseError $ "No such postfix operator: " ++ op

-- ref 11.2.3
evalFunCall :: Expr -> [Expr] -> Runtime JSVal
evalFunCall f args = do
  ref <- runExprStmt f
  argList <- evalArguments args
  callFunction ref argList

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


-- ref 7.8.5
makeRegularExpression :: String -> String -> Runtime JSVal
makeRegularExpression body flags = runExprStmt (NewExpr (ReadVar "RegExp") [ (Str body), (Str flags) ])

-- ref 11.1.5
makeObjectLiteral :: [PropertyAssignment] -> Runtime JSVal
makeObjectLiteral nameValueList =do
  cstr <- getGlobalProperty "Object"
  obj <- newObject >>= addOwnProperty "constructor" cstr
  mapM_ (addObjectProp obj) nameValueList
  return (VObj obj)

  where
    addObjectProp :: Shared JSObj -> PropertyAssignment -> Runtime (Shared JSObj)
    addObjectProp obj (name, value) = do
      desc <- makeDescriptor value
      objGetOwnProperty name obj >>= \case
        Just previous -> checkCompatible previous desc
        Nothing -> return ()
      defineOwnProperty name desc False obj
      return obj

    makeDescriptor :: PropertyValue -> Runtime (PropDesc JSVal)
    makeDescriptor (Value e) = do
      val <- runExprStmt e >>= getValue
      return $ dataPD val True True True
    makeDescriptor (Getter body) = do
      strict <- getGlobalStrictness
      env <- lexEnv <$> getGlobalContext
      func <- createFunction Nothing [] strict body env >>= mkGetter
      return $ accessorPD func Nothing True True
    makeDescriptor (Setter param body) = do
      strict <- getGlobalStrictness
      env <- lexEnv <$> getGlobalContext
      func <- createFunction Nothing [param] strict body env >>= mkSetter
      return $ accessorPD Nothing func True True

    checkCompatible :: PropDesc JSVal -> PropDesc JSVal -> Runtime ()
    checkCompatible a b = do
      let fail = raiseSyntaxError "Cannot reassign property"
      strict <- (== Strict) <$> getGlobalStrictness
      when (strict && isDataDescriptor (Just a) && isDataDescriptor (Just b)) fail
      when (isDataDescriptor (Just a) && isAccessorDescriptor (Just b)) fail
      when (isAccessorDescriptor (Just a) && isDataDescriptor (Just b)) fail
      when (isAccessorDescriptor (Just a) && isAccessorDescriptor (Just b)) $ do
        when (hasGetter a && hasGetter b) fail
        when (hasSetter a && hasSetter b) fail
      return ()


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
      VObj objRef ->
        callMethod <$> deref objRef >>= \case
          Nothing -> return "object"
          Just _  -> return "function"
      VNative{}   -> return "function"
      _ ->
        return $ case typeof resolved of
          TypeUndefined -> "undefined"
          TypeNull      -> "object"
          TypeBoolean   -> "boolean"
          TypeNumber    -> "number"
          TypeString    -> "string"
          _ -> showVal resolved
    return $ VStr result
