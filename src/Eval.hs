{-# LANGUAGE LambdaCase #-}

module Eval (runJS, evalJS, jsEvalExpr, runtime) where

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


evalJS :: String -> String -> IO (Either JSError (Maybe JSVal))
evalJS sourceName input = do
  runJS' sourceName input >>= \case
    ((Left err, _), _) -> return $ Left err
    ((Right v, _), _)  -> return $ Right v

runJS :: String -> String -> IO (Either JSError String)
runJS sourceName input = do
  r <- runJS' sourceName input
  case r of
    ((Left err, _), _)     -> return $ Left err
    ((Right _, output), _) -> return $ Right output

runJS' :: String -> String -> IO ((Either JSError (Maybe JSVal), String), JSGlobal)
runJS' sourceName input = case parseJS' input sourceName of
  Left err -> return ((Left ("SyntaxError", VStr $ show err, []), ""), emptyGlobal)
  Right ast -> runJSRuntime (runProg ast)

jsEvalExpr :: String -> IO JSVal
jsEvalExpr input = do
  result <- runtime $ do
    initGlobals
    cxt <- initialCxt
    runExprStmt cxt (parseExpr input) >>= getValue
  case result of
    Left err  -> error (show err)
    Right val -> return val

evalCode :: String -> JSRuntime StmtReturn
evalCode text = do
  case parseJS' text "(eval)" of
    Left err -> raiseError $ "SyntaxError: " ++ show err
    Right (Program strictness stmts) -> do
      cxt <- JSCxt <$> initialEnv
                   <*> emptyEnv
                   <*> (VObj <$> getGlobalObject)

      runStmts cxt stmts

runtime :: JSRuntime a -> IO (Either JSError a)
runtime p = do
  result <- runJSRuntime p
  return $ fst (fst result)


runJSRuntime :: JSRuntime a -> IO ((Either JSError a, String), JSGlobal)
runJSRuntime a = runStateT (runWriterT $ runExceptT $ unJS a) emptyGlobal

initGlobals :: JSRuntime ()
initGlobals = do
  newGlobalObject <- createGlobalThis
  modify $ \st -> st { globalObject = Just newGlobalObject,
                       globalEvaluator = Just evalCode,
                       globalRun = Just runStmts }


runProg :: Program -> JSRuntime (Maybe JSVal)
runProg (Program strictness stmts) = do
  initGlobals
  cxt <- initialCxt
  result <- runStmts cxt stmts
  case result of
    (CTNormal, v, _) -> return v
    (CTThrow, Just (VException exc@(excType, err, trace)), _) -> do
      msg <- callToString cxt err
      printStackTrace exc
      throwError (excType, msg, trace)
    (CTThrow, Just v, _) -> do
      v' <- callToString cxt v
      throwError ("Error", v', [])
    _ -> do
      liftIO $ putStrLn $ "Abnormal exit: " ++ show result
      return Nothing

callToString :: JSCxt -> JSVal -> JSRuntime JSVal
callToString _ (VStr str) = return (VStr str)
callToString cxt v = do
  obj <- toObject cxt v
  ref <- memberGet obj "toString"
  funCall cxt ref []




returnThrow :: Statement -> JSError -> JSRuntime StmtReturn
returnThrow s (excType, err, trace) =
 let exc = VException (excType, err, sourceLocation s : trace)
 in return (CTThrow, Just exc, Nothing)

runStmts :: JSCxt -> [Statement] -> JSRuntime StmtReturn
runStmts = runStmts' (CTNormal, Nothing, Nothing) where
  runStmts' emptyResult _ [] = return emptyResult
  runStmts' _ cxt (s:stmts) = do
    result <- runStmt cxt s `catchError` returnThrow s
    case result of
      (CTNormal, _, _) -> runStmts' result cxt stmts
      _ -> return result

runStmt :: JSCxt -> Statement -> JSRuntime StmtReturn
runStmt cxt s = case s of
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

  LabelledStatement loc label stmt -> runStmt cxt stmt

  IfStatement loc predicate ifThen ifElse -> do -- ref 12.5
    v <- runExprStmt cxt predicate >>= getValue
    if toBoolean v
    then runStmt cxt ifThen
    else case ifElse of
           Nothing -> return (CTNormal, Nothing, Nothing)
           Just s  -> runStmt cxt s

  DoWhileStatement loc e s -> -- ref 12.6.1
    runStmt cxt $ transformDoWhileToWhile loc e s

  WhileStatement loc e stmt -> keepGoing Nothing where -- ref 12.6.2
    keepGoing :: Maybe JSVal -> JSRuntime StmtReturn
    keepGoing v = do
      willEval <- isTruthy <$> (runExprStmt cxt e >>= getValue)

      if not willEval
      then return (CTNormal, v, Nothing)
      else do
        sval@(stype, v', _) <- runStmt cxt stmt
        let nextVal = case v' of
                        Nothing -> v
                        Just newVal -> Just newVal
        case stype of
          CTBreak -> return (CTNormal, v, Nothing)
          CTContinue -> keepGoing nextVal
          CTNormal -> keepGoing nextVal
          _ -> return sval

  For loc (For3 e1 e2 e3) stmt -> -- ref 12.6.3
    runStmt cxt $ transformFor3ToWhile loc e1 e2 e3 stmt

  For loc (For3Var x e1 e2 e3) stmt -> -- ref 12.6.3
    runStmt cxt $ transformFor3VarToWhile loc x e1 e2 e3 stmt

  Block loc stmts -> runStmts cxt stmts

  TryStatement _loc block catch finally -> do -- ref 12.14
    b@(btype, bval, _) <- runStmt cxt block
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

  _ -> error ("Unimplemented stmt: " ++ show s)

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
runCatch :: JSCxt -> Maybe Catch -> JSVal -> JSRuntime StmtReturn
runCatch _ Nothing _ = return (CTNormal, Nothing, Nothing)
runCatch cxt (Just (Catch _loc var stmt)) exc = do
  -- XXX create new environment
  putVar cxt var exc
  runStmt cxt stmt

maybeValList :: JSCxt -> [Maybe Expr] -> JSRuntime [Maybe JSVal]
maybeValList cxt = mapM (evalOne cxt)
  where
    evalOne cxt v = case v of
      Nothing -> return Nothing
      Just e  -> Just <$> (runExprStmt cxt e >>= getValue)

readVar :: JSCxt -> Ident -> Strictness -> JSRuntime JSVal
readVar cxt name strictness =
  VRef <$> getIdentifierReference (Just $ lexEnv cxt) name strictness

runExprStmt :: JSCxt -> Expr -> JSRuntime JSVal
runExprStmt cxt expr = case expr of
  Num n              -> return $ VNum n
  Str s              -> return $ VStr s
  Boolean b          -> return $ VBool b
  LiteralNull        -> return VNull
  ReadVar name       -> readVar cxt name NotStrict
  ReadVarStrict name -> readVar cxt name Strict
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

evalArrayLiteral :: JSCxt -> [Maybe Expr] -> JSRuntime JSVal
evalArrayLiteral cxt vals = createArray =<< mapM evalMaybe vals
  where evalMaybe Nothing = return Nothing
        evalMaybe (Just e) = Just <$> (runExprStmt cxt e >>= getValue)

evalArguments :: JSCxt -> [Expr] -> JSRuntime [JSVal]
evalArguments cxt = mapM (runExprStmt cxt >=> getValue)

modifyingOp :: (JSNum->JSNum) -> (JSNum->JSNum) -> JSCxt -> Expr -> JSRuntime JSVal
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
    _ -> raiseError $ "ReferenceError: " ++ show e ++ " is not assignable"

purePrefix :: (JSVal -> JSRuntime JSVal) -> JSCxt -> Expr -> JSRuntime JSVal
purePrefix f cxt e = runExprStmt cxt e >>= getValue >>= f



makeObjectLiteral :: JSCxt -> [(PropertyName, Expr)] -> JSRuntime JSVal
makeObjectLiteral cxt nameValueList =
  let nameOf :: PropertyName -> String
      nameOf (IdentProp ident) = ident
      nameOf (StringProp str)  = str
      nameOf (NumProp n)       = show n
      addProp :: Shared JSObj -> (PropertyName, Expr) -> JSRuntime (Shared JSObj)
      addProp obj (propName, propValue) = do
        val <- runExprStmt cxt propValue >>= getValue
        addOwnProperty (nameOf propName) val obj
  in do
    obj <- newObject
    VObj <$> foldM addProp obj nameValueList


evalTypeof :: JSVal -> JSRuntime JSVal
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
