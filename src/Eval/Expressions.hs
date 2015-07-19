{-# LANGUAGE LambdaCase #-}

module Eval.Expressions (evalExpr) where

import Control.Lens
import Control.Monad (void, when, replicateM)
import CompiledExpr
import Runtime
import Expr


type Stack = [JSVal]
type OpCodeHandler a = Stack -> (Stack -> Runtime a) -> Runtime a

debugOp :: CompiledExpr -> Runtime () -> Runtime ()
debugOp code action = do
  stack <- use valueStack
  debug code
  debug ("before", stack)
  v <- action
  stack' <- use valueStack
  debug ("after", stack')
  return v

-- Missed pattern match for [v] means we have a compiler bug!
evalExpr :: CompiledExpr -> Runtime JSVal
evalExpr code = runOpCode code [] (\[v] -> return v)

runOpCode :: OpCode -> OpCodeHandler a
runOpCode op = case op of
  OpConst v        -> runConst v
  OpThis           -> runOpThis
  OpVar name       -> runOpVar name
  OpArray n        -> runArray n
  OpObjLit n       -> runObjLit n
  OpSparse n       -> runSparse n
  OpGet name       -> runOpGet name
  OpGet2           -> runOpGet2
  OpGetValue       -> runOpGetValue
  OpToBoolean      -> runOpToBoolean
  OpToNumber       -> runOpToNumber
  OpDiscard        -> runDiscard
  OpDup            -> runDup
  OpSwap           -> runSwap
  OpRoll3          -> runRoll3
  OpAdd            -> runAdd
  OpSub            -> runSub
  OpMul            -> runMul
  OpBinary op      -> runBinaryOp op
  OpUnary op       -> runUnaryOp op
  OpModify op      -> runModify op
  OpDelete         -> runDelete
  OpTypeof         -> runTypeof
  OpStore          -> runStore
  OpFunCall n      -> runFunCall n
  OpNewCall n      -> runNewCall n
  OpLambda         -> runLambda

  BasicBlock ops   -> runBasicBlock ops
  IfTrue op1 op2   -> runIfTrue runOpCode op1 op2
  Nop              -> \s c -> c s
  _                -> error $ "No such opcode: " ++ show op


runConst :: JSVal -> OpCodeHandler a
runConst v stack cont = cont (v:stack)

runBasicBlock :: [OpCode] -> OpCodeHandler a
runBasicBlock [] stack cont = cont stack
runBasicBlock (op:rest) stack cont = runOpCode op stack runRest
  where runRest s' = runBasicBlock rest s' cont

runOpThis :: OpCodeHandler a
runOpThis stack cont = do
  v <- thisBinding <$> getGlobalContext
  cont (v : stack)

runOpVar :: Ident -> OpCodeHandler a
runOpVar name stack cont = do
  cxt <- getGlobalContext
  val <- getIdentifierReference (Just $ lexEnv cxt) name (cxtStrictness cxt)
  cont (VRef val : stack)

runArray :: Int -> OpCodeHandler a
runArray n stack cont =
  let (values, s') = splitAt n stack
  in do
    v <- createArray (map Just values)
    cont (v : s')

runSparse :: Int -> OpCodeHandler a
runSparse n (length:rest) cont =
  let pairs 0 stack = return ([], stack)
      pairs n (v:k:rest) = do
        k' <- toInt k
        (ps, s') <- pairs (n-1) rest
        return ((k',v):ps, s')
  in do
    (ps, s') <- pairs n rest
    v <- createSparseArray length ps
    cont (v : s')


runObjLit :: Int -> OpCodeHandler a
runObjLit n stack cont =
  let pairs 0 stack = ([], stack)
      pairs n (v:VStr k:rest) = ((k,v):ps, s')
        where (ps, s') = pairs (n-1) rest
      (ps, s') = pairs n stack
  in do
    v <- createObjectLiteral ps
    cont (v : s')



-- ref 11.2.1
runOpGet :: Ident -> OpCodeHandler a
runOpGet name (baseValue:rest) cont = do
  checkObjectCoercible ("Cannot read property " ++ name) baseValue
  v <- memberGet baseValue name
  cont (v : rest)

-- ref 11.2.1
runOpGet2 :: OpCodeHandler a
runOpGet2 (propertyNameValue:baseValue:rest) cont = do
  checkObjectCoercible ("Cannot read property " ++ showVal (propertyNameValue)) baseValue
  propertyNameString <- toString propertyNameValue
  v <- memberGet baseValue propertyNameString
  cont (v : rest)

runOpGetValue :: OpCodeHandler a
runOpGetValue (v:rest) cont = do
  v' <- getValue v
  cont (v' : rest)

runOpToBoolean :: OpCodeHandler a
runOpToBoolean = frob (VBool . toBoolean)

runOpToNumber :: OpCodeHandler a
runOpToNumber (v:rest) cont = case v of
    VInt _ -> cont (v:rest)
    VNum _ -> cont (v:rest)
    other  -> do
      n <- toNumber other
      cont (VNum n:rest)

runAdd :: OpCodeHandler a
runAdd (VInt n:VInt m:rest) cont = cont (VInt (m+n) : rest)
runAdd (e2:e1:rest) cont = do { result <- jsAdd e1 e2 ; cont (result : rest) }

runSub :: OpCodeHandler a
runSub (VInt n:VInt m:rest) cont = cont (VInt (m-n) : rest)
runSub (e2:e1:rest) cont = do { result <- numberOp (-) e1 e2 ; cont (result : rest) }

runMul :: OpCodeHandler a
runMul (VInt n:VInt m:rest) cont = cont (VInt (m*n) : rest)
runMul (e2:e1:rest) cont = do { result <- numberOp (*) e1 e2 ; cont (result : rest) }

runBinaryOp :: Ident -> OpCodeHandler a
runBinaryOp op (e2:e1:rest) cont = do
  result <- evalBinOp op e1 e2
  cont (result : rest)

runUnaryOp :: Ident -> OpCodeHandler a
runUnaryOp op (e : rest) cont = do
  v <- f e
  cont (v : rest)
  where
    f = case op of
          "+"    -> unaryPlus
          "-"    -> unaryMinus
          "!"    -> unaryNot
          "~"    -> unaryBitwiseNot
          "void" -> (return . const VUndef)
          _      -> const $ raiseError $ "Prefix not implemented: " ++ op

runModify :: Ident -> OpCodeHandler a
runModify op (val : rest) cont =
  let go f g = case val of
                 VNum n -> cont (VNum (f n) : rest)
                 VInt n -> cont (VInt (g n) : rest)
                 v      -> do { n <- toNumber v; cont (VNum (f n) : rest) }

  in case op of
            "++" -> go (+ 1) (+ 1)
            "--" -> go (subtract 1) (subtract 1)
            _    -> raiseError $ "No such modifier: " ++ op

-- ref 11.4.1
runDelete :: OpCodeHandler a
runDelete (val:rest) cont
  | not (isReference val) = cont (VBool True : rest)
  | isUnresolvableReference ref && isStrictReference ref = raiseSyntaxError "Delete of an unqualified identifier in strict mode (1)"
  | isUnresolvableReference ref = cont (VBool True : rest)
  | isPropertyReference ref = do { v <- deleteFromObj ref ; cont (v : rest) }
  | isStrictReference ref = raiseSyntaxError "Delete of an unqualified identifier in strict mode (2)"
  | otherwise = do { v <- deleteFromEnv ref ; cont (v : rest) }
  where
      ref = unwrapRef val
      deleteFromObj (JSRef base name strict) = do
        obj <- toObject base
        objDelete name (strict == Strict) obj
      deleteFromEnv (JSRef (VEnv base) name _strict) = deleteBinding name base

-- ref 11.4.3
runTypeof :: OpCodeHandler a
runTypeof (val:rest) cont =
  if isReference val && isUnresolvableReference (unwrapRef val)
  then cont (VStr "undefined" : rest)
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
    cont (VStr result : rest)

runStore :: OpCodeHandler a
runStore (val:lhs:rest) cont =
  case lhs of
    VRef ref -> putValue ref val >> cont (val : rest)
    _        -> raiseReferenceError $ show lhs ++ " is not assignable"

-- ref 11.2.2
runNewCall :: Int -> OpCodeHandler a
runNewCall n stack cont =
  let (args,(funcRef:rest)) = splitAt n stack
  in do
    func <- getValue funcRef
    assertFunction (show func) (view cstrMethod) func  -- XXX need to get the name here
    v <- newObjectFromConstructor func (reverse args)
    cont (VObj v : rest)


-- ref 11.2.3
runFunCall :: Int -> OpCodeHandler a
runFunCall n stack cont =
  let (args,(func:rest)) = splitAt n stack
  in do
    v <- callFunction func (reverse args)
    cont (v : rest)

runLambda :: OpCodeHandler a
runLambda stack cont = do
  (VLambda name params strict body, rest) <- pop stack
  env <- lexEnv <$> getGlobalContext
  func <- createFunction name params strict body env
  cont (func : rest)

runDiscard :: OpCodeHandler a
runDiscard (a:rest) cont = cont rest

runDup :: OpCodeHandler a
runDup (a:rest) cont = cont (a:a:rest)

runSwap :: OpCodeHandler a
runSwap (a:b:rest) cont = cont (b:a:rest)

runRoll3 :: OpCodeHandler a
runRoll3 (a:b:c:rest) cont = cont (b:c:a:rest)

runIfTrue :: (CompiledExpr -> OpCodeHandler a) -> CompiledExpr -> CompiledExpr -> OpCodeHandler a
runIfTrue eval ifTrue ifFalse stack cont = do
  (val, s') <- pop stack
  if val == VBool True
  then eval ifTrue s' cont
  else eval ifFalse s' cont

runIfEq :: JSVal -> OpCodeHandler a -> OpCodeHandler a
runIfEq val action stack cont = do
  (tos, s') <- pop stack
  if tos == val
  then action s' cont
  else cont s'

push :: JSVal -> OpCodeHandler a
push v stack cont = cont (v:stack)

pop :: Stack -> Runtime (JSVal, Stack)
pop (x:xs) = return (x, xs)
pop []     = raiseError "Stack underflow"

frob :: (JSVal -> JSVal) -> OpCodeHandler a
frob f [] cont = cont []
frob f (x:xs) cont = cont (f x : xs)

topOfStack :: Runtime JSVal
topOfStack = head <$> use valueStack
