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

    getStackTrace :: JSVal -> Runtime [SrcLoc]
    getStackTrace (VObj obj) = do
      st <- objGet "stack" obj
      return $ case st of
        VStacktrace s -> s
        _ -> []
    getStackTrace _ = return []

-- ref 12.1
runStmts :: [Statement] -> Runtime StmtReturn
runStmts = runStmt . desugar


runStmt :: CoreStatement -> Runtime StmtReturn
runStmt stmt = action `catchError` returnThrow stmt
  where
    action = case stmt of
      CoreBind dbiType bindings stmt -> runCoreBinding dbiType bindings stmt
      CoreBlock stmts                -> runCoreBlock stmts
      CoreLoop _loc test inc body    -> runCoreLoop test inc body
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
      case r of
        CTBreak    v _ -> return $ CTNormal v
        CTContinue v _ -> increment >> keepGoing v
        CTNormal   v   -> increment >> keepGoing v
        otherwise      -> return r





setStacktrace :: JSVal -> [SrcLoc] -> Runtime ()
setStacktrace v stack =
  case v of
    VObj objRef -> void $ addOwnProperty "stack" (VStacktrace stack) objRef
    _           -> return ()

runUnconvertedStmt :: Statement -> Runtime StmtReturn
runUnconvertedStmt s = {-# SCC stmt #-} case s of
  FunDecl {} -> return (CTNormal Nothing)
  ExprStmt _loc e -> {-# SCC "exprStmt" #-} do
    val <- runExprStmt e >>= getValue
    return $ CTNormal (Just val)

  VarDecl _loc assignments -> runVarDecl assignments -- ref 12.2
  LabelledStatement _loc _label stmt -> runStmt (Unconverted stmt)

  IfStatement _loc predicate ifThen ifElse -> do -- ref 12.5
    v <- runExprStmt predicate >>= getValue
    if toBoolean v
    then runStmt (Unconverted ifThen)
    else case ifElse of
           Nothing -> return $ CTNormal Nothing
           Just stmt  -> runStmt (Unconverted stmt)

  DoWhileStatement _loc e stmt -> -- ref 12.6.1
    runStmt . Unconverted $ transformDoWhileToWhile _loc e stmt

  WhileStatement _loc e stmt -> -- ref 12.6.2
    let cond      = toBoolean <$> (runExprStmt e >>= getValue)
        increment = return ()
    in loopConstruct cond increment stmt

  For loc (For3 e1 e2 e3) stmt -> -- ref 12.6.3
    let init =      case e1 of
                       Just e -> runExprStmt e >>= getValue >> return ()
                       Nothing -> return ()
        cond =      case e2 of
                       Just e  -> toBoolean <$> (runExprStmt e >>= getValue)
                       Nothing -> return True
        increment = case e3 of
                       Just e -> runExprStmt e >>= getValue >> return ()
                       Nothing -> return ()
    in init >> {-# SCC for3 #-} loopConstruct cond increment stmt


  For loc (For3Var assigns e2 e3) stmt -> -- ref 12.6.3
    runStmt . Unconverted $ transformFor3VarToFor3 loc assigns e2 e3 stmt

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
            s <- runStmt (Unconverted stmt)
            let nextv = rval s <|> v
            case s of
              CTBreak v' _    -> return $ CTNormal nextv
              CTContinue v' _ -> keepGoing obj ps  nextv
              CTNormal _      -> keepGoing obj ps  nextv
              _ -> return s
          else keepGoing obj ps v


  For loc (ForInVar (x, e1) e2) stmt ->
    runStmt . Unconverted $ transformFor3VarIn loc x e1 e2 stmt

  WithStatement _loc e s -> runWithStatement e s
  SwitchStatement _loc e caseBlock -> runSwitchStatement e caseBlock
  Block _loc stmts -> runStmts stmts

  TryStatement _loc block catch finally -> do -- ref 12.14
    br <- runStmt (Unconverted block)

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

  BreakStatement _loc label    -> return $ CTBreak    Nothing label
  ContinueStatement _loc label -> return $ CTContinue Nothing label
  Return _loc Nothing          -> return $ CTReturn  (Just VUndef)
  Return _loc (Just e) -> do
    val <- runExprStmt e >>= getValue
    return $ CTReturn (Just val)

  EmptyStatement _loc -> return $ CTNormal Nothing

  _ -> error ("Unimplemented stmt: " ++ show s)


loopConstruct :: Runtime Bool -> Runtime () -> Statement -> Runtime StmtReturn
loopConstruct condition increment stmt = keepGoing Nothing where
  keepGoing :: Maybe JSVal -> Runtime StmtReturn
  keepGoing v = do
    willEval <- condition
    if not willEval
    then return $ CTNormal v
    else do
      r <- {-# SCC loop_body #-} runStmt (Unconverted stmt)
      case r of
        CTBreak    v _ -> return $ CTNormal v
        CTContinue v _ -> increment >> keepGoing v
        CTNormal   v   -> increment >> keepGoing v
        otherwise      -> return r


-- |
-- Turn "for (var x = e1; e2; e3) { s }" into
-- "var x = e1; while (e2) { s; e3 }"
-- with sensible defaults for missing statements
transformFor3VarToFor3 :: SrcLoc -> [VarDeclaration] -> Maybe Expr -> Maybe Expr -> Statement -> Statement
transformFor3VarToFor3 loc decls e2 e3 stmt =
  let s1 = VarDecl loc decls
      s2 = For loc (For3 Nothing e2 e3) stmt
  in Block loc [ s1, s2 ]

-- |
-- Turn "do { s } while (e)" into
-- "while (true) { s; if (!e) break; }"
transformDoWhileToWhile :: SrcLoc -> Expr -> Statement -> Statement
transformDoWhileToWhile loc e s =
  let esc = IfStatement loc (UnOp "!" e)
              (BreakStatement loc Nothing)
              Nothing
  in WhileStatement loc (Boolean True) $ TryStatement loc s Nothing (Just $ Finally loc esc)

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
  withLexEnv newEnv $ runStmt (Unconverted s)

-- ref 12.11
runSwitchStatement :: Expr -> CaseBlock -> Runtime StmtReturn
runSwitchStatement e caseBlock = do
  val <- runExprStmt e >>= getValue
  r <- runCaseBlock val caseBlock
  return $ case r of
   CTBreak v _ -> CTNormal v
   otherwise   -> r

runCaseBlock :: JSVal -> CaseBlock -> Runtime StmtReturn
runCaseBlock val caseBlock = do
  step5 caseBlock Nothing False
  where
    step5 :: CaseBlock -> Maybe JSVal -> Bool -> Runtime StmtReturn
    step5 (as, d, bs) v found = case as of

        (a@(CaseClause e stmts) : rest) -> do
              if not found
              then do clauseSelector <- runExprStmt e >>= getValue
                      if clauseSelector `eqv` val
                      then step5 (a:rest, d, bs) v True
                      else step5 (  rest, d, bs) v False
              else do r <- runStmts stmts
                      let nextv = rval r <|> v
                      case r of
                        CTNormal _ -> step5 (rest, d, bs) nextv True
                        otherwise  -> return r { rval = nextv }

        [] -> if found
              then step8 ([], d, bs) v
              else step7 ([], d, bs) v False

    step7 :: CaseBlock -> Maybe JSVal -> Bool -> Runtime StmtReturn
    step7 (as, d, bs) v foundInB = case (foundInB, bs) of

      (False, (b@(CaseClause e stmts) : rest)) -> do
              clauseSelector <- runExprStmt e >>= getValue
              if clauseSelector `eqv` val
              then step7 (as, d, b:rest) v True
              else step7 (as, d,   rest) v foundInB

      _ -> if foundInB
           then step9 (as, d, bs) v
           else step8 (as, d, bs) v


    step8 :: CaseBlock -> Maybe JSVal -> Runtime StmtReturn
    step8 (as, d, bs) v = case d of

      Just (DefaultClause stmts) -> do
              r <- runStmts stmts
              let nextv = rval r <|> v
              case r of
                CTNormal _ -> step9 (as, d, bs) nextv
                otherwise  -> return r { rval = nextv }

      _ -> step9 (as, d, bs) v

    step9 :: CaseBlock -> Maybe JSVal -> Runtime StmtReturn
    step9 (as, d, bs) v = case bs of

      (b@(CaseClause e stmts) : rest) -> do
              r <- runStmts stmts
              let nextv = rval r <|> v
              case r of
                CTNormal _ -> step9 ([], d, rest) nextv
                otherwise  -> return r { rval = nextv }

      [] -> return (CTNormal v)







-- ref 12.14
runCatch :: Statement -> JSVal -> Runtime StmtReturn
runCatch (Catch _loc var block) c = do
  oldEnv <- lexEnv <$> getGlobalContext
  catchEnv <- newDeclarativeEnvironment (Just oldEnv)
  rec <- envRec <$> deref catchEnv
  createMutableBinding var True rec
  setMutableBinding var c False rec
  withLexEnv catchEnv $ runStmt (Unconverted block)

runFinally :: Statement -> Runtime StmtReturn
runFinally (Finally _loc stmt) = runStmt (Unconverted stmt)

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
  LiteralUndefined   -> return VUndef
  ReadVar name       -> readVar name
  This               -> thisBinding <$> getGlobalContext
  ArrayLiteral vals  -> evalArrayLiteral vals
  ObjectLiteral map  -> makeObjectLiteral map

  MemberDot e x      -> runExprStmt (MemberGet e (Str x)) -- ref 11.2.1

  MemberGet e x -> do -- ref 11.2.1
    baseValue <- runExprStmt e >>= getValue
    propertyNameValue <- runExprStmt x >>= getValue
    checkObjectCoercible ("Cannot read property " ++ showVal (propertyNameValue)) baseValue
    propertyNameString <- toString propertyNameValue
    memberGet baseValue propertyNameString

  FunCall f args -> do  -- ref 11.2.3
    ref <- runExprStmt f
    argList <- evalArguments args
    callFunction ref argList

  Assign lhs op e -> do
    lref <- runExprStmt lhs
    rref <- runExprStmt e
    case op of
      "=" -> assignRef lref rref
      _   -> updateRef (init op) lref rref

  Cond e1 e2 e3 -> do
    lref <- runExprStmt e1 >>= getValue
    if toBoolean lref
    then runExprStmt e2 >>= getValue
    else runExprStmt e3 >>= getValue

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

  NewExpr f args -> do -- ref 11.2.2
    fun <- runExprStmt f >>= getValue
    argList <- evalArguments args
    assertFunction (show f) cstrMethod fun  -- XXX need to get the name here
    liftM VObj (newObjectFromConstructor fun argList)

  FunExpr name params strictness body -> do
    env <- lexEnv <$> getGlobalContext
    createFunction name params strictness body env

  _ -> error ("Unimplemented expr: " ++ show expr)

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
