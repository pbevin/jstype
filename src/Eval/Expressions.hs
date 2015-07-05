{-# LANGUAGE LambdaCase #-}

module Eval.Expressions (runCompiledExpr, evalExpr) where

import Control.Lens
import Control.Monad (void, when, replicateM)
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
  OpArray n        -> runArray n
  OpNewObj n       -> runNewObj n
  OpSparse n       -> runSparse n
  OpGet name       -> runOpGet name
  OpGet2           -> runOpGet2
  OpGetValue       -> runOpGetValue
  OpToBoolean      -> runOpToBoolean
  OpToNumber       -> runOpToNumber
  OpDiscard        -> void pop
  OpDup            -> topOfStack >>= push
  OpSwap           -> runSwap
  OpRoll3          -> runRoll3
  OpBinary op      -> runBinaryOp op
  OpUnary op       -> runUnaryOp op
  OpModify op      -> runModify op
  OpDelete         -> runDelete
  OpTypeof         -> runTypeof
  OpStore          -> runStore
  OpFunCall n      -> runFunCall n
  OpNewCall n      -> runNewCall n

  BasicBlock ops   -> mapM_ (runCompiledExpr globalEval) ops
  IfTrue op1 op2   -> runIfTrue (runCompiledExpr globalEval) op1 op2
  Interpreted expr -> push =<< globalEval expr
  Nop              -> return ()
  _                -> error $ "No such opcode: " ++ show op

evalExpr :: (Expr -> Runtime JSVal) -> CompiledExpr -> Runtime JSVal
evalExpr globalEval code = runCompiledExpr globalEval code >> pop

runOpThis :: Runtime ()
runOpThis = push =<< (thisBinding <$> getGlobalContext)

runOpVar :: Ident -> Runtime ()
runOpVar name = do
  cxt <- getGlobalContext
  val <- getIdentifierReference (Just $ lexEnv cxt) name (cxtStrictness cxt)
  push (VRef val)

runArray :: Int -> Runtime ()
runArray n = do
  values <- replicateM n pop
  push =<< createArray (map Just values)

runSparse :: Int -> Runtime ()
runSparse n = do
  length <- pop
  values <- replicateM n $ do
    v <- pop
    k <- toInt =<< pop
    return (k,v)
  push =<< createSparseArray length values

runNewObj :: Int -> Runtime ()
runNewObj n = do
  values <- replicateM n $ do
    v <- pop
    VStr k <- pop
    return (k, v)
  push =<< createObjectLiteral values



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

runOpGetValue :: Runtime ()
runOpGetValue = push =<< getValue =<< pop

runOpToBoolean :: Runtime ()
runOpToBoolean = frob (VBool . toBoolean)

runOpToNumber :: Runtime ()
runOpToNumber = pop >>= toNumber >>= push . VNum

runBinaryOp :: Ident -> Runtime ()
runBinaryOp op =
  let action = evalBinOp op
  in do e2 <- pop
        e1 <- pop
        action e1 e2 >>= push

runUnaryOp :: Ident -> Runtime ()
runUnaryOp op = do
  e <- pop
  push =<< f e
  where
    f = case op of
          "+"    -> unaryPlus
          "-"    -> unaryMinus
          "!"    -> unaryNot
          "~"    -> unaryBitwiseNot
          "void" -> (return . const VUndef)
          _      -> const $ raiseError $ "Prefix not implemented: " ++ op

runModify :: Ident -> Runtime ()
runModify op =
  let go f = pop >>= toNumber >>= push . VNum . f
  in case op of
            "++" -> go (+ 1)
            "--" -> go (subtract 1)
            _    -> raiseError $ "No such modifier: " ++ op

-- modifyingOp :: (JSNum -> JSNum) -> Runtime ()
-- modifyingOp op = do
--   lhs <- pop
--   case lhs of
--     VRef ref -> do
--       lval <- getValue lhs
--       val <- toNumber lval
--       putValue ref . VNum $ op val
--       return ()
--     _ -> raiseReferenceError $ show lhs ++ " is not assignable"

-- ref 11.4.1
runDelete :: Runtime ()
runDelete = pop >>= runDelete'
  where runDelete' val
          | not (isReference val) = push (VBool True)
          | isUnresolvableReference ref && isStrictReference ref = raiseSyntaxError "Delete of an unqualified identifier in strict mode (1)"
          | isUnresolvableReference ref = push (VBool True)
          | isPropertyReference ref = push =<< deleteFromObj ref
          | isStrictReference ref = raiseSyntaxError "Delete of an unqualified identifier in strict mode (2)"
          | otherwise = push =<< deleteFromEnv ref
          where
              ref = unwrapRef val
              deleteFromObj (JSRef base name strict) = do
                obj <- toObject base
                objDelete name (strict == Strict) obj
              deleteFromEnv (JSRef (VEnv base) name _strict) = deleteBinding name base

-- ref 11.4.3
runTypeof :: Runtime ()
runTypeof = do
  val <- pop
  if isReference val && isUnresolvableReference (unwrapRef val)
  then push $ VStr "undefined"
  else do
    resolved <- getValue val
    result <- case resolved of
      VObj objRef ->
        (^.callMethod) <$> deref objRef >>= \case
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
    push $ VStr result

runStore :: Runtime ()
runStore = do
  val <- pop
  lhs <- pop
  case lhs of
    VRef ref -> putValue ref val
    _        -> raiseReferenceError $ show lhs ++ " is not assignable"
  push val

-- ref 11.2.2
runNewCall :: Int -> Runtime ()
runNewCall n = do
  func <- getValue =<< pop
  args <- reverse <$> replicateM n pop
  assertFunction "(function)" (view cstrMethod) func  -- XXX need to get the name here
  push =<< VObj <$> newObjectFromConstructor func args

-- ref 11.2.3
runFunCall :: Int -> Runtime ()
runFunCall n = do
  func <- pop
  args <- reverse <$> replicateM n pop
  push =<< callFunction func args



runSwap :: Runtime ()
runSwap = do
  a <- pop
  b <- pop
  push a
  push b

runRoll3 :: Runtime ()
runRoll3 = do
  a <- pop
  b <- pop
  c <- pop
  push a
  push c
  push b

runIfTrue :: (CompiledExpr -> Runtime ()) -> CompiledExpr -> CompiledExpr -> Runtime ()
runIfTrue eval ifTrue ifFalse = do
  val <- pop
  if val == VBool True
  then eval ifTrue
  else eval ifFalse


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

frob :: (JSVal -> JSVal) -> Runtime ()
frob f = pop >>= push . f

topOfStack :: Runtime JSVal
topOfStack = head <$> use valueStack
