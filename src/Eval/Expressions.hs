module Eval.Expressions (runCompiledExpr, evalExpr) where

import Control.Lens
import Control.Monad (void, when)
import CompiledExpr
import Runtime
import Expr


debugOp :: CompiledExpr -> Runtime () -> Runtime ()
debugOp code action = do
  stack <- use valueStack
  debug code
  debug ("before", stack)
  v <- action
  stack' <- use valueStack
  debug ("after", stack')
  return v

runCompiledExpr :: (Expr -> Runtime JSVal) -> CompiledExpr -> Runtime ()
runCompiledExpr globalEval op = case op of
  OpConst v        -> push v
  OpThis           -> runOpThis
  OpVar name       -> runOpVar name
  OpGet name       -> runOpGet name
  OpGet2           -> runOpGet2
  OpToValue        -> runOpToValue
  OpToBoolean      -> runOpToBoolean
  OpDiscard        -> void pop
  OpDup            -> topOfStack >>= push
  OpBinary op      -> runBinaryOp op
  BasicBlock ops   -> mapM_ (runCompiledExpr globalEval) ops
  IfEq val code    -> runIfEq val (runCompiledExpr globalEval code)
  Interpreted expr -> push =<< globalEval expr
  _                -> error $ "No such opcode: " ++ show op

evalExpr :: (Expr -> Runtime JSVal) -> CompiledExpr -> Runtime JSVal
evalExpr globalEval code = runCompiledExpr globalEval code >> pop

singleItemOnStack :: Runtime JSVal
singleItemOnStack = do
  stack <- use valueStack
  case stack of
    [val] -> pop >> return val
    _     -> error $ "Stack should have had 1 element; found " ++ show stack


runOpThis :: Runtime ()
runOpThis = push =<< (thisBinding <$> getGlobalContext)

runOpVar :: Ident -> Runtime ()
runOpVar name = do
  cxt <- getGlobalContext
  val <- getIdentifierReference (Just $ lexEnv cxt) name (cxtStrictness cxt)
  push (VRef val)

-- ref 11.2.1
runOpGet :: Ident -> Runtime ()
runOpGet name = do
  baseValue <- pop
  checkObjectCoercible ("Cannot read property " ++ name) baseValue
  memberGet baseValue name >>= push

-- ref 11.2.1
runOpGet2 :: Runtime ()
runOpGet2 = do
  propertyNameValue <- pop
  baseValue         <- pop
  checkObjectCoercible ("Cannot read property " ++ showVal (propertyNameValue)) baseValue
  propertyNameString <- toString propertyNameValue
  memberGet baseValue propertyNameString >>= push

runOpToValue :: Runtime ()
runOpToValue = frob getValue

runOpToBoolean :: Runtime ()
runOpToBoolean = frob (return . VBool . toBoolean)

runBinaryOp :: Ident -> Runtime ()
runBinaryOp op =
  let action = evalBinOp op
  in do e2 <- pop
        e1 <- pop
        action e1 e2 >>= push

runIfEq :: JSVal -> Runtime () -> Runtime ()
runIfEq val action = do
  tos <- pop
  when (tos == val) action

push :: JSVal -> Runtime ()
push v = do
  valueStack %= (v:)

pop :: Runtime JSVal
pop = do
  v <- topOfStack
  valueStack %= tail
  return v

frob :: (JSVal -> Runtime JSVal) -> Runtime ()
frob f = pop >>= f >>= push

topOfStack :: Runtime JSVal
topOfStack = head <$> use valueStack
