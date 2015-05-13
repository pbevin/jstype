
module Eval where

import Control.Monad.State
import Control.Monad.Except
import Control.Monad.Writer
import Control.Applicative
import Data.Maybe
import Data.List (intercalate)
import Data.IORef
import qualified Data.Map as M
import Text.Show.Functions
import Parse
import Expr
import Runtime.Types
import Runtime.Object
import Runtime.Reference
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
  Right ast -> runJSRuntime (liftIO initialEnv >>= runProg ast)

jsEvalExpr :: String -> IO JSVal
jsEvalExpr input = do
  env <- initialEnv
  result <- runJSRuntime (runExprStmt env $ parseExpr input)
  case result of
    (Left err, _)  -> error (show err)
    (Right val, _) -> return val

runJSRuntime :: JSRuntime a -> IO (Either JSError a, String)
runJSRuntime a = runWriterT (runExceptT (unJS a))




runProg :: Program -> JSEnv -> JSRuntime ()
runProg (Program stmts) env = forM_ stmts (runStmt env)

runStmt :: JSEnv -> Statement -> JSRuntime ()
runStmt env s = case s of
  ExprStmt e -> void $ runExprStmt env e

  VarDecl assignments ->
    forM_ assignments $ \(x, e) -> case e of
      Nothing  -> putVar env x VUndef
      Just e' -> do { v <- runExprStmt env e'; putVar env x v }

  For (For3 e1 e2 e3) stmt ->
    maybeRunExprStmt env e1 >> keepGoing where
      keepGoing = do
        willEval <- case e2 of
          Nothing  -> pure True
          Just e2' -> isTruthy <$> runExprStmt env e2'

        if willEval
        then do
          runStmt env stmt
          maybeRunExprStmt env e3
          keepGoing
        else return ()

  Block stmts -> forM_ stmts (runStmt env)

  EmptyStatement -> return ()
  _ -> error ("Unimplemented stmt: " ++ show s)

maybeRunExprStmt :: JSEnv -> Maybe Expr -> JSRuntime ()
maybeRunExprStmt _ Nothing  = return ()
maybeRunExprStmt env (Just e) = void (runExprStmt env e)


runExprStmt :: JSEnv -> Expr -> JSRuntime JSVal
runExprStmt env expr = case expr of
  Num n          -> return $ VNum n
  Str s          -> return $ VStr s
  ReadVar x      -> lookupVar env x

  MemberDot e x  -> do
    lval <- runExprStmt env e >>= getValue
    case lval of
      VMap m -> return $ maybe VUndef id $ M.lookup x m
      VObj _ -> return $ VRef (JSRef lval x False)
      _ -> error $ "Can't do ." ++ x ++ " on " ++ show lval

  FunCall f args -> do  -- ref 11.2.3
    ref <- runExprStmt env f
    func <- getValue ref
    argList <- evalArguments env args
    let thisValue = computeThisValue ref
    objCall env func thisValue argList

  Assign lhs op e -> do
    lref <- runExprStmt env lhs
    rref <- runExprStmt env e
    updateRef (assignOp op) lref rref

  BinOp op e1 e2 -> do
    evalBinOp op <$> (runExprStmt env e1 >>= getValue)
                 <*> (runExprStmt env e2 >>= getValue)

  PostOp op e -> do -- ref 11.3
    lhs <- runExprStmt env e
    lval <- getValue lhs
    putValue lhs (postfixUpdate op lval)
    return lval

  NewExpr f args -> do
    if f == ReadVar "Error"
    then JSErrorObj <$> (runExprStmt env $ head args)
    else newObject >>= return . VObj

  FunDef (Just name) params body -> do
    fun <- createFunction params body env
    putVar env name fun
    return fun

  FunDef Nothing params body -> createFunction params body env

  _              -> error ("Unimplemented expr: " ++ show expr)

evalArguments :: JSEnv -> [Expr] -> JSRuntime [JSVal]
evalArguments env = mapM (\a -> runExprStmt env a >>= getValue)

computeThisValue :: JSVal -> JSVal
computeThisValue v = case v of
  VRef ref ->
    if isPropertyReference ref
    then getBase ref
    else VUndef -- s/b ImplicitThisRef

  _ -> VUndef


putVar :: JSEnv -> Ident -> JSVal -> JSRuntime ()
putVar envref x v = liftIO $ do
  env <- readIORef envref
  let valref = M.lookup x env
  case valref of
    Nothing -> do
      newRef <- newIORef v
      writeIORef envref (M.insert x newRef env)
    Just ref  -> writeIORef ref v
  return ()

lookupVar :: JSEnv -> Ident -> JSRuntime JSVal
lookupVar envref x = do
  return $ VRef $ JSRef (VEnv envref) x False


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

initialEnv :: IO JSEnv
initialEnv = do
  console <- newIORef $ VMap $ M.fromList [ ("log", VNative jsConsoleLog) ]
  newIORef $ M.fromList [ ("console", console) ]

jsConsoleLog :: JSVal -> [JSVal] -> JSRuntime JSVal
jsConsoleLog _this xs = tell ((intercalate " " $ map showVal xs) ++ "\n") >> return VUndef

showVal :: JSVal -> String
showVal (VStr s) = s
showVal (VNum (JSNum n)) = show (round n :: Integer)
showVal VUndef = "(undefined)"
showVal other = show other

postfixUpdate :: String -> JSVal -> JSVal
postfixUpdate "++" (VNum v) = VNum (v+1)
postfixUpdate "--" (VNum v) = VNum (v-1)
postfixUpdate op v = error $ "No such postfix op " ++ op ++ " on " ++ (show v)

evalBinOp :: String -> JSVal -> JSVal -> JSVal
evalBinOp op (VNum v1) (VNum v2) = case op of
  "+" -> VNum $ v1 + v2
  "-" -> VNum $ v1 - v2
  "*" -> VNum $ v1 * v2
  "/" -> VNum $ v1 / v2
  "<" -> VBool $ v1 < v2
  _ -> error $ "No binop " ++ op ++ " on " ++ show (v1, v2)
evalBinOp "+" (VStr a) (VStr b) = VStr (a ++ b)
evalBinOp op v1 v2 = error $ "No binop " ++ op ++ " on " ++ show (v1, v2)







-------------------------------------------------

createFunction :: [Ident] -> [Statement] -> JSEnv -> JSRuntime JSVal
createFunction paramList body env = do
    objref <- newObject
    liftIO $ modifyIORef objref $
      \obj -> obj { objClass = "Function", callMethod = funcCall env paramList body }
    return $ VObj objref


funcCall :: JSEnv -> [Ident] -> [Statement] -> JSVal -> [JSVal] -> JSRuntime JSVal
funcCall env paramList body this args =
  let makeRef name = JSRef (VEnv env) name False
      refs = map makeRef paramList
  in do
    zipWithM_ putEnvironmentRecord refs args
    forM_ body (runStmt env)
    return VUndef


prim :: PrimitiveFunction -> JSVal
prim = VPrim

objCall :: JSEnv -> JSVal -> JSVal -> [JSVal] -> JSRuntime JSVal
objCall env func this args = case func of
  VNative f -> f this args
  VObj objref -> liftIO (readIORef objref) >>= \obj -> (callMethod obj) this args
  _ -> error $ "Can't call " ++ show func
