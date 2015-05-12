{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Eval where

import Control.Monad.State
import Control.Monad.Except
import Control.Monad.Writer
import Control.Applicative
import Data.List (intercalate)
import Data.IORef
import qualified Data.Map as M
import Text.Show.Functions
import Parse
import Expr

import Debug.Trace

data JSVal = VNum JSNum
           | VStr String
           | VBool Bool
           | VRef (IORef JSVal)
           | VUndef
           | VMap (M.Map Ident JSVal)
           | VNative ([JSVal] -> JSRuntime JSVal)
           | VFun [Ident] [Statement]
           | JSVoid
           | JSErrorObj JSVal

instance Show JSVal where
  show (VNum a) = show a
  show (VStr a) = show a
  show (VBool a) = show a
  show (VRef _) = "(reference)"
  show VUndef = "undefined"
  show (VMap _) = "(map)"
  show (VNative _) = "(native function)"
  show (VFun _ _) = "(userdef function)"
  show JSVoid = "void"
  show (JSErrorObj a) = "JSError(" ++ show a ++ ")"



instance Eq JSVal where
  VNum a == VNum b = a == b
  VStr a == VStr b = a == b
  a == b = error $ "Can't compare " ++ show a ++ " and " ++ show b

type JSOutput = String
type JSError = String
type JSEnv = IORef (M.Map Ident (IORef JSVal))

newtype JSRuntime a = JS {
  unJS :: ExceptT JSError (WriterT String IO) a
} deriving (Monad, MonadIO, MonadWriter String, MonadError JSError, Functor, Applicative)

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
    lref <- runExprStmt env e
    VMap m <- getValue lref
    return $ maybe VUndef id $ M.lookup x m

  FunCall f args -> do
    f' <- runExprStmt env f >>= getValue
    args' <- mapM (\a -> runExprStmt env a >>= getValue) args
    funCall env f' args'

  Assign lhs op e -> do
    lref <- runExprStmt env lhs
    rref <- runExprStmt env e
    updateRef (assignOp op) lref rref

  BinOp op e1 e2 -> do
    evalBinOp op <$> (runExprStmt env e1 >>= getValue)
                 <*> (runExprStmt env e2 >>= getValue)

  PostOp op e -> do
    lref <- runExprStmt env e
    let f = postfixOp op
    case lref of
      VRef v -> do
        liftIO $ modifyIORef' v f
        return lref
      _ -> error $ "Can't postop " ++ show lref

  NewExpr f args -> do
    if f == ReadVar "Error"
    then JSErrorObj <$> (runExprStmt env $ head args)
    else error "Can only new Error() so far"

  FunDef (Just name) params body -> do
    let fun = VFun params body
    putVar env name fun
    return fun

  _              -> error ("Unimplemented expr: " ++ show expr)

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

lookupVar :: JSEnv -> Ident -> JSRuntime (JSVal)
lookupVar envref x = do
  env <- liftIO $ readIORef envref
  let valref = M.lookup x env
  case valref of
    -- Nothing -> error $ "No such variable " ++ x
    Nothing -> do
      newRef <- liftIO $ newIORef VUndef
      liftIO $ writeIORef envref (M.insert x newRef env)
      return $ VRef newRef
    Just ref  -> return $ VRef ref


updateRef :: (JSVal -> JSVal -> JSVal) -> JSVal -> JSVal -> JSRuntime JSVal
updateRef f lref rref = do
  lval <- getValue lref
  rval <- getValue rref
  let r = f lval rval
  putValue lref r

getValue :: JSVal -> JSRuntime JSVal
getValue v = liftIO $
  case v of
    VRef ref -> do
      val <- readIORef ref
      return val
    other -> return other

putValue :: JSVal -> JSVal -> JSRuntime JSVal
putValue (VRef ref) val = do
  liftIO $ writeIORef ref val
  return val
putValue _ _ = throwError "ReferenceError"


funCall :: JSEnv -> JSVal -> [JSVal] -> JSRuntime JSVal
funCall env func args = case func of
  VNative f -> f args

  VFun params body -> do
    forM_ (zip params args) $ \(x,v) -> do
      putVar env x v
    mapM_ (runStmt env) body
    return VUndef

  _ -> error $ "Can't call: " ++ show func


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

jsConsoleLog :: [JSVal] -> JSRuntime JSVal
jsConsoleLog xs = tell ((intercalate " " $ map showVal xs) ++ "\n") >> return VUndef

showVal :: JSVal -> String
showVal (VStr s) = s
showVal (VNum (JSNum n)) = show (round n :: Integer)
showVal VUndef = "(undefined)"
showVal other = show other

postfixOp :: String -> JSVal -> JSVal
postfixOp "++" (VNum v) = VNum (v+1)
postfixOp "--" (VNum v) = VNum (v-1)
postfixOp op v = error $ "No such postfix op " ++ op ++ " on " ++ (show v)

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
