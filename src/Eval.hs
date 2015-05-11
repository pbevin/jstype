{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Eval where

import Control.Monad.State
import Control.Monad.Except
import Control.Monad.Writer
import Control.Applicative
import qualified Data.Map as M
import Text.Show.Functions
import Parse
import Expr

import Debug.Trace

data JSVal = VNum JSNum
           | VStr String
           | VBool Bool
           | VRef Ident
           | VUndef
           | VMap (M.Map Ident JSVal)
           | VNative ([JSVal] -> JSRuntime JSVal)
           | VFun [Ident] [Statement]
           | JSVoid
           | JSErrorObj JSVal
           deriving Show

instance Eq JSVal where
  VNum a == VNum b = a == b

type JSOutput = String
type JSError = String
type JSEnv = M.Map Ident JSVal

newtype JSRuntime a = JS {
  unJS :: ExceptT JSError (WriterT String (State JSEnv)) a
} deriving (Monad, MonadState JSEnv, MonadError String, MonadWriter String, Functor, Applicative)

instance Num JSNum where
  (JSNum a) + (JSNum b) = JSNum (a + b)
  (JSNum a) - (JSNum b) = JSNum (a - b)
  (JSNum a) * (JSNum b) = JSNum (a * b)
  fromInteger n = JSNum $ fromInteger n
  abs (JSNum a) = JSNum $ abs a
  signum (JSNum a) = JSNum $ signum a

instance Fractional JSNum where
  (JSNum a) / (JSNum b) = JSNum (a / b)
  fromRational r = JSNum $ fromRational r

instance Ord JSNum where
  (JSNum a) < (JSNum b) = a < b

instance Num JSVal where
  (VNum a) + (VNum b) = VNum (a + b)
  (VNum a) - (VNum b) = VNum (a - b)
  (VNum a) * (VNum b) = VNum (a * b)
  fromInteger n = VNum $ fromInteger n
  abs (VNum a) = VNum $ (abs a)
  signum (VNum a) = VNum $ signum a

instance Fractional JSVal where
  (VNum a) / (VNum b) = VNum (a / b)
  fromRational r = VNum $ fromRational r

runJS :: String -> Either JSError String
runJS input = case runJS' input of
  (Left err, _, _)     -> Left err
  (Right _, output, _) -> Right output

runJS' :: String -> (Either JSError (), JSOutput, JSEnv)
runJS' input = case parseJS input of
  Left err -> error (show err)
  Right ast -> runJSRuntime (runProg ast)

jsEvalExpr :: String -> JSVal
jsEvalExpr input = case runJSRuntime (runExprStmt $ parseExpr input) of
  (Left err, _, _)  -> error (show err)
  (Right val, _, _) -> val

runJSRuntime :: JSRuntime a -> (Either JSError a, JSOutput, JSEnv)
runJSRuntime a = let ((result, output), env) = runState (runWriterT (runExceptT (unJS a))) initialEnv
                 in (result, output, env)

runProg :: Program -> JSRuntime ()
runProg (Program stmts) = forM_ stmts runStmt

runStmt :: Statement -> JSRuntime ()
runStmt s = case s of
  ExprStmt e -> void $ runExprStmt e

  VarDecl assignments ->
    forM_ assignments $ \(x, e) -> case e of
      Nothing -> putVar x VUndef
      Just e  -> do { v <- runExprStmt e; putVar x v }

  For (For3 e1 e2 e3) stmt ->
    maybeRunExprStmt e1 >> iterate where
      iterate = do
        willEval <- case e2 of
          Nothing  -> pure True
          Just e2' -> isTruthy <$> runExprStmt e2'

        if willEval
        then do
          runStmt stmt
          maybeRunExprStmt e3
          iterate
        else return ()

  Block stmts -> forM_ stmts runStmt

  EmptyStatement -> return ()
  _ -> error ("Unimplemented stmt: " ++ show s)

maybeRunExprStmt :: Maybe Expr -> JSRuntime ()
maybeRunExprStmt Nothing  = return ()
maybeRunExprStmt (Just e) = void (runExprStmt e)


runExprStmt :: Expr -> JSRuntime JSVal
runExprStmt expr = case expr of
  Num n          -> return $ VNum n
  Str s          -> return $ VStr s
  ReadVar x      -> return $ VRef x

  MemberDot e x  -> do
    lref <- runExprStmt e
    VMap m <- getValue lref
    return $ maybe VUndef id $ M.lookup x m

  FunCall f args -> do
    f' <- runExprStmt f >>= getValue
    args' <- mapM (\a -> runExprStmt a >>= getValue) args
    funCall f' args'

  Assign lhs op e -> do
    lref <- runExprStmt lhs
    rref <- runExprStmt e
    updateRef (assignOp op) lref rref

  BinOp op e1 e2 -> do
    evalBinOp op <$> (runExprStmt e1 >>= getValue)
                 <*> (runExprStmt e2 >>= getValue)

  PostOp op (ReadVar x) -> do
    updateVar (+1) x

  NewExpr f args -> do
    if f == ReadVar "Error"
    then JSErrorObj <$> (runExprStmt $ head args)
    else error "Can only new Error() so far"

  FunDef (Just name) params body -> do
    let fun = VFun params body
    putVar name fun
    return fun

  _              -> error ("Unimplemented expr: " ++ show expr)

putVar :: Ident -> JSVal -> JSRuntime ()
putVar x v = do
  env <- get
  put (M.insert x v env)
  return ()


updateRef :: (JSVal -> JSVal -> JSVal) -> JSVal -> JSVal -> JSRuntime JSVal
updateRef f lref rref = do
  lval <- getValue lref
  rval <- getValue rref
  let r = f lval rval
  putValue lref r

getValue :: JSVal -> JSRuntime JSVal
getValue (VRef x) = get >>= return . maybe VUndef id . M.lookup x
getValue other = return other

putValue :: JSVal -> JSVal -> JSRuntime JSVal
putValue (VRef id) val = do
  env <- get
  put $ M.insert id val env
  return val
putValue _ _ = throwError "ReferenceError"


updateVar :: (JSVal -> JSVal) -> Ident -> JSRuntime JSVal
updateVar f x = do
  env <- get

  let old = maybe VUndef id $ M.lookup x env
  let new = f old

  put $ M.insert x new env

  return new

funCall :: JSVal -> [JSVal] -> JSRuntime JSVal
funCall (VNative f) args = f args
funCall (VFun params body) args = do
  env <- get
  forM_ (zip params args) $ \(x,v) -> do
    putVar x v
  mapM_ runStmt body
  put $ env
  return VUndef
funCall other args = error $ "Can't call: " ++ show other


assignOp :: String -> (JSVal -> JSVal -> JSVal)
assignOp "=" = const id
assignOp "+=" = (+)
assignOp "-=" = (-)
assignOp "*=" = (*)
assignOp "/=" = (/)
assignOp other = error $ "No assignOp for " ++ show other

isTruthy :: JSVal -> Bool
isTruthy (VNum 0)      = False
isTruthy VUndef        = False
isTruthy (VBool False) = False
isTruthy _             = True

initialEnv :: JSEnv
initialEnv = M.fromList [ ("console", VMap $ M.fromList [ ("log", VNative jsConsoleLog) ]) ]

jsConsoleLog :: [JSVal] -> JSRuntime JSVal
jsConsoleLog [n] = tell (showVal n ++ "\n") >> return n

showVal :: JSVal -> String
showVal (VStr s) = s
showVal (VNum (JSNum n)) = show (round n :: Integer)
showVal VUndef = "(undefined)"
showVal other = show other

evalBinOp :: String -> JSVal -> JSVal -> JSVal
evalBinOp op (VNum v1) (VNum v2) = case op of
  "+" -> VNum $ v1 + v2
  "-" -> VNum $ v1 - v2
  "*" -> VNum $ v1 * v2
  "/" -> VNum $ v1 / v2
  "<" -> VBool $ v1 < v2
  other -> error $ "No such binop: " ++ other
