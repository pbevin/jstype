module Eval.Expressions (compileExpr, runCompiledExpr, evalExpr) where

import Control.Lens
import Control.Monad (void, when)
import Runtime
import Expr

data CompiledExpr = OpConst JSVal    -- push constant
                   | OpThis          -- push current `this`
                   | OpVar Ident     -- read variable, push reference
                   | OpGet Ident     -- property access with identifier
                   | OpGet2          -- property access with value
                   | OpToValue       -- dereference top of stack
                   | OpToBoolean     -- convert TOS to boolean
                   | OpDiscard       -- discard TOS
                   | OpDup           -- duplicate TOS
                   | BasicBlock [CompiledExpr]
                   | IfEq JSVal CompiledExpr
                   | Interpreted Expr
                   deriving Show


compileExpr :: Expr -> CompiledExpr
compileExpr expr = case expr of
  Num n            -> OpConst (VNum n)
  Str s            -> OpConst (VStr s)
  Boolean b        -> OpConst (VBool b)
  LiteralNull      -> OpConst (VNull)
  LiteralUndefined -> OpConst (VUndef)
  ReadVar v        -> OpVar v
  This             -> OpThis
  MemberDot e x    -> compilePropAccDot e x
  MemberGet e x    -> compilePropAccBracket e x
  BinOp "&&" e1 e2 -> compileShortCircuitAnd e1 e2
  BinOp "||" e1 e2 -> compileShortCircuitOr e1 e2
  _                -> Interpreted expr


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
  BasicBlock ops   -> mapM_ (runCompiledExpr globalEval) ops
  IfEq val code    -> runIfEq val (runCompiledExpr globalEval code)
  Interpreted expr -> push =<< globalEval expr
  _                -> error $ "No such opcode: " ++ show op

evalExpr :: (Expr -> Runtime JSVal) -> Expr -> Runtime JSVal
evalExpr globalEval expr = runCompiledExpr globalEval (compileExpr expr) >> pop

singleItemOnStack :: Runtime JSVal
singleItemOnStack = do
  stack <- use valueStack
  case stack of
    [val] -> pop >> return val
    _     -> error $ "Stack should have had 1 element; found " ++ show stack

compilePropAccDot :: Expr -> Ident -> CompiledExpr
compilePropAccDot expr ident = BasicBlock [ compileExpr expr, OpToValue, OpGet ident ]

compilePropAccBracket :: Expr -> Expr -> CompiledExpr
compilePropAccBracket expr ix = BasicBlock [ compileExpr ix, OpToValue, compileExpr expr, OpToValue, OpGet2 ]

compileShortCircuitAnd :: Expr -> Expr -> CompiledExpr
compileShortCircuitAnd e1 e2 =
  let compe1 = [ compileExpr e1, OpToValue, OpDup, OpToBoolean ]
      compe2 = [ OpDiscard, compileExpr e2, OpToValue ]
  in BasicBlock $ compe1 ++ [ IfEq (VBool True) (BasicBlock compe2) ]

compileShortCircuitOr :: Expr -> Expr -> CompiledExpr
compileShortCircuitOr e1 e2 =
  let compe1 = [ compileExpr e1, OpToValue, OpDup, OpToBoolean]
      compe2 = [ OpDiscard, compileExpr e2, OpToValue ]
  in BasicBlock $ compe1 ++ [ IfEq (VBool False) (BasicBlock compe2) ]


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
  baseValue         <- pop
  propertyNameValue <- pop
  checkObjectCoercible ("Cannot read property " ++ showVal (propertyNameValue)) baseValue
  propertyNameString <- toString propertyNameValue
  memberGet baseValue propertyNameString >>= push

runOpToValue :: Runtime ()
runOpToValue = frob getValue

runOpToBoolean :: Runtime ()
runOpToBoolean = frob (return . VBool . toBoolean)


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
