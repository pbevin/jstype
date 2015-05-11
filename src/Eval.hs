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
           | VUndef
           | VMap (M.Map Ident JSVal)
           | VNative ([JSVal] -> JSRuntime JSVal)
           | JSVoid
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
        else return ()


  _ -> error ("Unimplemented stmt: " ++ show s)

maybeRunExprStmt :: Maybe Expr -> JSRuntime ()
maybeRunExprStmt Nothing  = return ()
maybeRunExprStmt (Just e) = void (runExprStmt e)


runExprStmt :: Expr -> JSRuntime JSVal
runExprStmt expr = case expr of
  Num n          -> return $ VNum n
  ReadVar x      -> lookupVar x
  MemberDot e x  -> do
    VMap m <- runExprStmt e
    return $ maybe VUndef id $ M.lookup x m

  FunCall f args -> do
    f' <- runExprStmt f
    args' <- mapM runExprStmt args
    funCall f' args'

  Assign (ReadVar x) op e -> do
    v <- runExprStmt e
    env <- get
    let old = maybe VUndef id $ M.lookup x env
    let new = assignOp op old v
    put $ M.insert x new env

    return new

  BinOp op e1 e2 -> do
    evalBinOp op <$> runExprStmt e1 <*> runExprStmt e2

  _              -> error ("Unimplemented expr: " ++ show expr)

lookupVar :: Ident -> JSRuntime JSVal
lookupVar x = do
  env <- get
  case M.lookup x env of
    Nothing -> return $ VNum 0 -- XXX
    Just x  -> return x

putVar :: Ident -> JSVal -> JSRuntime ()
putVar x v = do
  env <- get
  put (M.insert x v env)
  return ()

funCall :: JSVal -> [JSVal] -> JSRuntime JSVal
funCall (VNative f) args = f args

assignOp :: String -> (JSVal -> JSVal -> JSVal)
assignOp "=" = const id
assignOp "+=" = (+)
assignOp "-=" = (-)
assignOp "*=" = (*)
assignOp "/=" = (/)


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
showVal (VNum (JSNum n)) = show (round n :: Integer)
showVal VUndef = "(undefined)"

evalBinOp :: String -> JSVal -> JSVal -> JSVal
evalBinOp op (VNum v1) (VNum v2) = VNum $ case op of
  "+" -> v1 + v2
  "-" -> v1 - v2
  "*" -> v1 * v2
  "/" -> v1 / v2
