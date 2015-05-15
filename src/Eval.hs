module Eval where

import Control.Monad.State
import Control.Monad.Except
import Control.Monad.Writer
import Control.Applicative
import Data.Maybe
import Data.List (intercalate)
import qualified Data.Foldable as F
import qualified Data.Map as M
import Text.Show.Functions
import Parse
import Expr
import Runtime.Types
import Runtime.Object
import Runtime.Reference
import Runtime.Conversion
import Debug.Trace

runJStr :: String -> IO (Either JSError String)
runJStr = runJS ""


runJS :: String -> String -> IO (Either JSError String)
runJS sourceName input = do
  r <- runJS' sourceName input
  case r of
    (Left err, _)     -> return $ Left err
    (Right _, output) -> return $ Right output

runJS' :: String -> String -> IO (Either JSError (), String)
runJS' sourceName input = case parseJS' input sourceName of
  Left err -> error (show err)
  Right ast -> runJSRuntime (runProg ast)

jsEvalExpr :: String -> IO JSVal
jsEvalExpr input = do
  result <- runJSRuntime (initialCxt >>= \cxt -> runExprStmt cxt $ parseExpr input)
  case result of
    (Left err, _)  -> error (show err)
    (Right val, _) -> return val

runJSRuntime :: JSRuntime a -> IO (Either JSError a, String)
runJSRuntime a = runWriterT (runExceptT (unJS a))

initialCxt :: JSRuntime JSCxt
initialCxt = JSCxt <$> initialEnv <*> emptyEnv <*> (VObj <$> newObject)

initialEnv :: JSRuntime JSEnv
initialEnv = do
  console <- newObject
  modifyRef console $ objSetProperty "log" (VNative jsConsoleLog)

  function <- newObject
  modifyRef function $ \obj -> obj { callMethod = Just funConstructor }

  share $ M.fromList [ ("console", VObj console),
                       ("Function", VObj function) ]

emptyEnv :: JSRuntime JSEnv
emptyEnv = share M.empty


runProg :: Program -> JSRuntime ()
runProg (Program stmts) = do
  cxt <- initialCxt
  result <- runStmts cxt stmts
  case result of
    (CTNormal, _, _) -> return ()
    (CTThrow, Just (VException (err, trace)), _) -> stackTrace (err, trace)
    _ -> liftIO $ putStrLn $ "Abnormal exit: " ++ show result

returnThrow :: Statement -> JSError -> JSRuntime (CompletionType, Maybe JSVal, Maybe Ident)
returnThrow s (err, trace) = return (CTThrow, Just $ VException (err, (sourceLocation s):trace), Nothing)

runStmts :: JSCxt -> [Statement] -> JSRuntime (CompletionType, Maybe JSVal, Maybe Ident)
runStmts _ [] = return (CTNormal, Nothing, Nothing)
runStmts cxt (s:stmts) = do
  result <- runStmt cxt s `catchError` returnThrow s
  case result of
    (CTNormal, _, _) -> runStmts cxt stmts
    _ -> return result

runStmt :: JSCxt -> Statement -> JSRuntime (CompletionType, Maybe JSVal, Maybe Ident)
runStmt cxt s = case s of
  ExprStmt loc e -> do
    val <- runExprStmt cxt e
    return (CTNormal, Just val, Nothing)

  VarDecl loc assignments -> do
    F.forM_ assignments $ \(x, e) -> case e of
      Nothing  -> putVar cxt x VUndef
      Just e' -> do { v <- runExprStmt cxt e'; putVar cxt x v }
    return (CTNormal, Nothing, Nothing)

  For loc (For3 e1 e2 e3) stmt -> do
    maybeRunExprStmt cxt e1 >> keepGoing where
      keepGoing = do
        willEval <- case e2 of
          Nothing  -> pure True
          Just e2' -> isTruthy <$> runExprStmt cxt e2'

        if willEval
        then do
          runStmt cxt stmt
          maybeRunExprStmt cxt e3
          keepGoing
        else return (CTNormal, Nothing, Nothing)

  IfStatement loc predicate ifThen ifElse -> do -- ref 12.5
    v <- runExprStmt cxt predicate >>= getValue
    if toBoolean v
    then runStmt cxt ifThen
    else case ifElse of
           Nothing -> return (CTNormal, Nothing, Nothing)
           Just s  -> runStmt cxt s

  Block loc stmts -> runStmts cxt stmts

  Return loc Nothing -> return (CTReturn, Just VUndef, Nothing)
  Return loc (Just e) -> do
    val <- runExprStmt cxt e
    return (CTReturn, Just val, Nothing)

  EmptyStatement loc -> return (CTNormal, Nothing, Nothing)

  _ -> error ("Unimplemented stmt: " ++ show s)

maybeRunExprStmt :: JSCxt -> Maybe Expr -> JSRuntime ()
maybeRunExprStmt _ Nothing  = return ()
maybeRunExprStmt cxt (Just e) = void (runExprStmt cxt e)






runExprStmt :: JSCxt -> Expr -> JSRuntime JSVal
runExprStmt cxt expr = case expr of
  Num n          -> return $ VNum n
  Str s          -> return $ VStr s
  ReadVar x      -> lookupVar cxt x
  This           -> return $ thisBinding cxt

  MemberDot e x  -> do
    lval <- runExprStmt cxt e >>= getValue
    case lval of
      VMap m -> return $ fromMaybe VUndef $ M.lookup x m
      VObj _ -> return $ VRef (JSRef lval x False)
      _ -> raiseError $ "Can't do ." ++ x ++ " on " ++ show lval

  FunCall f args -> do  -- ref 11.2.3
    ref <- runExprStmt cxt f
    func <- getValue ref
    argList <- evalArguments cxt args
    let thisValue = computeThisValue ref
    objCall cxt func thisValue argList

  Assign lhs op e -> do
    lref <- runExprStmt cxt lhs
    rref <- runExprStmt cxt e
    updateRef (assignOp op) lref rref

  BinOp op e1 e2 -> do
    v1 <- (runExprStmt cxt e1 >>= getValue)
    v2 <- (runExprStmt cxt e2 >>= getValue)
    evalBinOp op v1 v2

  UnOp "typeof" e -> do -- ref 11.4.3
    val <- runExprStmt cxt e >>= getValue
    return $ VStr $ case typeof val of
      TypeUndefined -> "undefined"
      TypeNull      -> "null"
      TypeBoolean   -> "boolean"
      TypeNumber    -> "number"
      TypeString    -> "string"
      TypeObject    -> "object"  -- or "function"

  PostOp op e -> do -- ref 11.3
    lhs <- runExprStmt cxt e
    lval <- getValue lhs
    putValue lhs (postfixUpdate op lval)
    return lval

  NewExpr f args ->
    if f == ReadVar "Error"
    then JSErrorObj <$> runExprStmt cxt (head args)
    else do
      fun <- runExprStmt cxt f >>= getValue
      argList <- evalArguments cxt args
      liftM VObj (newObjectFromConstructor cxt fun argList)

  FunDef (Just name) params body -> do
    fun <- createFunction params body cxt
    putVar cxt name fun
    return fun

  FunDef Nothing params body -> createFunction params body cxt

  _              -> error ("Unimplemented expr: " ++ show expr)

evalArguments :: JSCxt -> [Expr] -> JSRuntime [JSVal]
evalArguments cxt = mapM (runExprStmt cxt >=> getValue)

computeThisValue :: JSVal -> JSVal
computeThisValue v = case v of
  VRef ref ->
    if isPropertyReference ref
    then getBase ref
    else VUndef -- s/b ImplicitThisRef

  _ -> VUndef


putVar :: JSCxt -> Ident -> JSVal -> JSRuntime ()
putVar (JSCxt envref _ _) x v = modifyRef envref $ M.insert x v

lookupVar :: JSCxt -> Ident -> JSRuntime JSVal
lookupVar envref x = return $ VRef $ JSRef (VCxt envref) x False


updateRef :: (JSVal -> JSVal -> JSVal) -> JSVal -> JSVal -> JSRuntime JSVal
updateRef f lref rref = do
  lval <- getValue lref
  rval <- getValue rref
  let r = f lval rval
  putValue lref r
  return r


assignOp :: String -> JSVal -> JSVal -> JSVal
assignOp "=" _ b = b
assignOp "+=" (VNum a) (VNum b) = VNum (a+b)
assignOp "-=" (VNum a) (VNum b) = VNum (a-b)
assignOp "*=" (VNum a) (VNum b) = VNum (a*b)
assignOp "/=" (VNum a) (VNum b) = VNum (a/b)
assignOp other a b = error $ "No assignOp for " ++ show other ++ " on " ++ show (a,b)

isTruthy :: JSVal -> Bool
isTruthy (VNum 0)      = False
isTruthy VUndef        = False
isTruthy (VBool False) = False
isTruthy _             = True


jsConsoleLog :: JSVal -> [JSVal] -> JSRuntime JSVal
jsConsoleLog _this xs = tell (unwords (map showVal xs) ++ "\n") >> return VUndef

showVal :: JSVal -> String
showVal (VStr s) = s
showVal (VNum (JSNum n)) = show (round n :: Integer)
showVal VUndef = "(undefined)"
showVal other = show other

postfixUpdate :: String -> JSVal -> JSVal
postfixUpdate "++" (VNum v) = VNum (v+1)
postfixUpdate "--" (VNum v) = VNum (v-1)
postfixUpdate op v = error $ "No such postfix op " ++ op ++ " on " ++ show v

evalBinOp :: String -> JSVal -> JSVal -> JSRuntime JSVal
evalBinOp op x@(VNum v1) y@(VNum v2) = case op of
  "+" -> return $ VNum $ v1 + v2
  "-" -> return $ VNum $ v1 - v2
  "*" -> return $ VNum $ v1 * v2
  "/" -> return $ VNum $ v1 / v2
  "<" -> return $ VBool $ v1 < v2
  "===" -> return $ tripleEquals x y
  _ -> raiseError $ "No binop " ++ op ++ " on " ++ show (v1, v2)
evalBinOp "===" x y = return $ tripleEquals x y
evalBinOp "+" (VStr a) (VStr b) = return $ VStr (a ++ b)
evalBinOp op v1 v2 = raiseError $ "No binop " ++ op ++ " on " ++ show (v1, v2)


-- ref 11.9.6, incomplete
tripleEquals :: JSVal -> JSVal -> JSVal
tripleEquals x y
  | typeof x /= typeof y        = VBool False
  | typeof x == TypeUndefined   = VBool True
  | typeof x == TypeNull        = VBool True
  | typeof x == TypeNumber      = VBool (x == y)
  | typeof x == TypeString      = VBool (x == y)
  | typeof x == TypeBoolean     = VBool (x == y)
  | typeof x == TypeObject      = VBool (x == y)



-------------------------------------------------

createFunction :: [Ident] -> [Statement] -> JSCxt -> JSRuntime JSVal
createFunction paramList body cxt = do
    objref <- newObject
    modifyRef objref $
      \obj -> obj { objClass = "Function",
                    callMethod = Just (funcCall cxt paramList body) }
    return $ VObj objref


-- ref 13.2.1, incomplete
funcCall :: JSCxt -> [Ident] -> [Statement] -> JSVal -> [JSVal] -> JSRuntime JSVal
funcCall cxt paramList body this args =
  let makeRef name = JSRef (VCxt cxt) name False
      refs = map makeRef paramList
      newCxt = cxt { thisBinding = this }
  in do
    zipWithM_ putEnvironmentRecord refs args
    result <- runStmts newCxt body
    case result of
      (CTReturn, Just v, _)  -> return v
      (CTThrow, Just (VException (err, trace)), _) -> throwError (err, trace)
      _ -> return VUndef

-- ref 13.2.2, incomplete
newObjectFromConstructor :: JSCxt -> JSVal -> [JSVal] -> JSRuntime (Shared JSObj)
newObjectFromConstructor cxt fun@(VObj funref) args = do
  obj <- newObject
  f <- deref funref
  prototype <- objGetProperty "prototype" f
  modifyRef obj $ objSetProperty "prototype" $ fromMaybe VUndef prototype
  objCall cxt fun (VObj obj) args
  return obj


funConstructor :: JSVal -> [JSVal] -> JSRuntime JSVal
funConstructor this [arg] =
  let body = toString arg
      params = []
      Program stmts = simpleParse body
  in createFunction params stmts =<< initialCxt
funConstructor this xs = error $ "Can't cstr Function with " ++ show xs

toString (VStr s) = s

prim :: PrimitiveFunction -> JSVal
prim = VPrim

objCall :: JSCxt -> JSVal -> JSVal -> [JSVal] -> JSRuntime JSVal
objCall cxt func this args = case func of
  VNative f -> f this args
  VObj objref -> deref objref >>= \obj -> fromJust (callMethod obj) this args
  _ -> error $ "Can't call " ++ show func


stackTrace :: JSError -> JSRuntime ()
stackTrace (err, trace) = liftIO $ do
  putStrLn $ "Error: " ++ err
  mapM_ print (reverse trace)
