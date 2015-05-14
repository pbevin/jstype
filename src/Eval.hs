module Eval where

import Control.Monad.State
import Control.Monad.Except
import Control.Monad.Writer
import Control.Applicative
import Data.Maybe
import Data.List (intercalate)
import Data.IORef
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

runJS :: String -> IO (Either JSError String)
runJS input = do
  r <- runJS' input
  case r of
    (Left err, _)     -> return $ Left err
    (Right _, output) -> return $ Right output

runJS' :: String -> IO (Either JSError (), String)
runJS' input = case parseJS input of
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
  liftIO $ modifyIORef console $ objSetProperty "log" (VNative jsConsoleLog)
  liftIO $ newIORef $ M.fromList [ ("console", VObj console) ]

emptyEnv :: JSRuntime JSEnv
emptyEnv = liftIO (newIORef M.empty)


runProg :: Program -> JSRuntime ()
runProg (Program stmts) = do
  cxt <- initialCxt
  F.forM_ stmts $ runStmt cxt

runStmt :: JSCxt -> Statement -> JSRuntime ()
runStmt cxt s = case s of
  ExprStmt e -> void $ runExprStmt cxt e

  VarDecl assignments ->
    F.forM_ assignments $ \(x, e) -> case e of
      Nothing  -> putVar cxt x VUndef
      Just e' -> do { v <- runExprStmt cxt e'; putVar cxt x v }

  For (For3 e1 e2 e3) stmt ->
    maybeRunExprStmt cxt e1 >> keepGoing where
      keepGoing = do
        willEval <- case e2 of
          Nothing  -> pure True
          Just e2' -> isTruthy <$> runExprStmt cxt e2'

        when willEval $ do
          runStmt cxt stmt
          maybeRunExprStmt cxt e3
          keepGoing

  IfStatement predicate ifThen ifElse -> do -- ref 12.5
    v <- runExprStmt cxt predicate >>= getValue
    if toBoolean v
    then runStmt cxt ifThen
    else F.forM_ ifElse (runStmt cxt)

  Block stmts -> F.forM_ stmts (runStmt cxt)

  EmptyStatement -> return ()

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
      _ -> error $ "Can't do ." ++ x ++ " on " ++ show lval

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

  BinOp op e1 e2 ->
    evalBinOp op <$> (runExprStmt cxt e1 >>= getValue)
                 <*> (runExprStmt cxt e2 >>= getValue)

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
putVar (JSCxt envref _ _) x v = liftIO $ do
  modifyIORef envref $ \env -> M.insert x v env

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

evalBinOp :: String -> JSVal -> JSVal -> JSVal
evalBinOp op x@(VNum v1) y@(VNum v2) = case op of
  "+" -> VNum $ v1 + v2
  "-" -> VNum $ v1 - v2
  "*" -> VNum $ v1 * v2
  "/" -> VNum $ v1 / v2
  "<" -> VBool $ v1 < v2
  "===" -> tripleEquals x y
  _ -> error $ "No binop " ++ op ++ " on " ++ show (v1, v2)
evalBinOp "===" x y = tripleEquals x y
evalBinOp "+" (VStr a) (VStr b) = VStr (a ++ b)
evalBinOp op v1 v2 = error $ "No binop " ++ op ++ " on " ++ show (v1, v2)


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
    liftIO $ modifyIORef objref $
      \obj -> obj { objClass = "Function",
                    callMethod = funcCall cxt paramList body }
    return $ VObj objref


-- ref 13.2.1, incomplete
funcCall :: JSCxt -> [Ident] -> [Statement] -> JSVal -> [JSVal] -> JSRuntime JSVal
funcCall cxt paramList body this args =
  let makeRef name = JSRef (VCxt cxt) name False
      refs = map makeRef paramList
      newCxt = cxt { thisBinding = this }
  in do
    zipWithM_ putEnvironmentRecord refs args
    F.forM_ body (runStmt newCxt)
    return VUndef

-- ref 13.2.2, incomplete
newObjectFromConstructor :: JSCxt -> JSVal -> [JSVal] -> JSRuntime (IORef JSObj)
newObjectFromConstructor cxt fun@(VObj funref) args = do
  obj <- newObject
  f <- liftIO $ readIORef funref
  prototype <- objGetProperty f "prototype"
  liftIO $ modifyIORef obj $ objSetProperty "prototype" $ fromMaybe VUndef prototype
  objCall cxt fun (VObj obj) args
  return obj



prim :: PrimitiveFunction -> JSVal
prim = VPrim

objCall :: JSCxt -> JSVal -> JSVal -> [JSVal] -> JSRuntime JSVal
objCall cxt func this args = case func of
  VNative f -> f this args
  VObj objref -> liftIO (readIORef objref) >>= \obj -> callMethod obj this args
  _ -> error $ "Can't call " ++ show func
