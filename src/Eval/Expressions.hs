{-# LANGUAGE LambdaCase, OverloadedStrings #-}

module Eval.Expressions (evalExpr) where

import Control.Lens
import Control.Monad (void, when, replicateM)
import Control.Monad.Except
import qualified Data.Text as T
import Data.Text (Text)
import Data.Monoid
import OpCodes
import Runtime
import Expr

type Stack = [JSVal]
data VMReturn = Continue Stack | RelativeBranch Int Stack deriving (Show, Eq)
type OpCodeHandler = Stack -> Runtime VMReturn
type PC = Int

emptyStack = []

cont = return . Continue
branch skip stack = return (RelativeBranch skip stack)

-- Missed pattern match for [v] means we have a compiler bug!
evalExpr :: CompiledExpr -> Runtime JSVal
evalExpr code = do
  stack <- runOpCodes code 0 emptyStack
  case stack of
    [v]       -> return v
    otherwise -> error ("Stack bug: " ++ show stack)

runOpCodes :: CompiledExpr -> PC -> Stack -> Runtime Stack
runOpCodes ops pc stack
  | pc >= length ops = return stack
  | otherwise = do
      result <- runOpCode (ops !! pc) stack
      case result of
        Continue s             -> runOpCodes ops (pc + 1) s
        RelativeBranch delta s -> runOpCodes ops (pc + 1 + delta) s

runOpCode :: OpCode -> OpCodeHandler
runOpCode op = case op of
  OpConst v        -> runConst v
  OpNum n          -> runConst (VNum n)
  OpInt n          -> runConst (VInt n)
  OpStr s          -> runConst (VStr s)
  OpBool b         -> runConst (VBool b)
  OpNull           -> runConst VNull
  OpUndef          -> runConst VUndef
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
  OpPostInc delta  -> runOpPostInc delta
  OpModify op      -> runModify op
  OpDelete         -> runDelete
  OpTypeof         -> runTypeof
  OpStore          -> runStore
  OpFunCall n      -> runFunCall n
  OpNewCall n      -> runNewCall n
  OpLambda         -> runLambda
  OpBranch n       -> runOpBranch n
  OpBFalse n       -> runOpCondBranch (== VBool False) n
  Nop              -> cont
  -- _                -> error $ "No such opcode: " ++ show op


runConst :: JSVal -> OpCodeHandler
runConst v stack = cont (v:stack)

runOpThis :: OpCodeHandler
runOpThis stack = do
  v <- thisBinding <$> getGlobalContext
  cont (v : stack)

runOpVar :: Ident -> OpCodeHandler
runOpVar name stack = do
  cxt <- getGlobalContext
  val <- getIdentifierReference (Just $ lexEnv cxt) name (cxtStrictness cxt)
  cont (VRef val : stack)

runArray :: Int -> OpCodeHandler
runArray n stack =
  let (values, s') = splitAt n stack
  in do
    v <- createArray (map Just values)
    cont (v : s')

runSparse :: Int -> OpCodeHandler
runSparse n (VInt length:rest) =
  let pairs 0 stack = return ([], stack)
      pairs n (v:k:rest) = do
        k' <- toInt k
        (ps, s') <- pairs (n-1) rest
        return ((k',v):ps, s')
      pairs _ stack = error $ "Stack underflow: " ++ show stack
  in do
    (ps, s') <- pairs n rest
    v <- createSparseArray (fromIntegral length) ps
    cont (v : s')
runSparse _ stack = error $ "Stack underflow: " ++ show stack


runObjLit :: Int -> OpCodeHandler
runObjLit n stack =
  let pairs 0 stack = ([], stack)
      pairs n (v:VStr k:rest) = ((k,v):ps, s')
        where (ps, s') = pairs (n-1) rest
      pairs _ stack = error $ "Stack underflow: " ++ show stack
      (ps, s') = pairs n stack
  in do
    v <- createObjectLiteral ps
    cont (v : s')


-- ref 11.2.1
runOpGet :: Ident -> OpCodeHandler
runOpGet name (baseValue:rest) = do
  checkObjectCoercible ("Cannot read property " ++ T.unpack name) baseValue
  v <- memberGet baseValue (T.unpack name)
  cont (v : rest)
runOpGet _ stack = error $ "Stack underflow: " ++ show stack

-- ref 11.2.1
runOpGet2 :: OpCodeHandler
runOpGet2 (propertyNameValue:baseValue:rest) = do
  checkObjectCoercible ("Cannot read property " ++ T.unpack (showVal propertyNameValue)) baseValue
  propertyNameString <- toString propertyNameValue
  v <- memberGet baseValue $ T.unpack propertyNameString
  cont (v : rest)
runOpGet2 stack = error $ "Stack underflow: " ++ show stack

runOpGetValue :: OpCodeHandler
runOpGetValue (v:rest) = do
  v' <- getValue v
  cont (v' : rest)
runOpGetValue stack = error $ "Stack underflow: " ++ show stack

runOpToBoolean :: OpCodeHandler
runOpToBoolean = frob (VBool . toBoolean)

runOpToNumber :: OpCodeHandler
runOpToNumber (v:rest) = case v of
    VInt _ -> cont (v:rest)
    VNum _ -> cont (v:rest)
    other  -> do
      n <- toNumber other
      cont (VNum n:rest)
runOpToNumber stack = error $ "Stack underflow: " ++ show stack

runAdd :: OpCodeHandler
runAdd (VInt n:VInt m:rest) = cont (VInt (m+n) : rest)
runAdd (e2:e1:rest) = do { result <- jsAdd e1 e2 ; cont (result : rest) }
runAdd stack = error $ "Stack underflow: " ++ show stack

runSub :: OpCodeHandler
runSub (VInt n:VInt m:rest) = cont (VInt (m-n) : rest)
runSub (e2:e1:rest) = do { result <- numberOp (-) e1 e2 ; cont (result : rest) }
runSub stack = error $ "Stack underflow: " ++ show stack

runMul :: OpCodeHandler
runMul (VInt n:VInt m:rest) = cont (VInt (m*n) : rest)
runMul (e2:e1:rest) = do { result <- numberOp (*) e1 e2 ; cont (result : rest) }
runMul stack = error $ "Stack underflow: " ++ show stack

runBinaryOp :: Ident -> OpCodeHandler
runBinaryOp op (e2:e1:rest) = do
  result <- evalBinOp op e1 e2
  cont (result : rest)
runBinaryOp _ stack = error $ "Stack underflow: " ++ show stack

runUnaryOp :: Ident -> OpCodeHandler
runUnaryOp op (e : rest) = do
  v <- f e
  cont (v : rest)
  where
    f = case op of
          "+"    -> unaryPlus
          "-"    -> unaryMinus
          "!"    -> unaryNot
          "~"    -> unaryBitwiseNot
          "void" -> (return . const VUndef)
          _      -> const $ raiseError $ "Prefix not implemented: " <> op
runUnaryOp _ stack = error $ "Stack underflow: " ++ show stack

-- ref 11.3.1
runOpPostInc :: Int -> OpCodeHandler
runOpPostInc delta (lhs : rest) = case lhs of
  VRef ref -> do
    oldValue <- getValue lhs >>= toNum
    newValue <- jsAdd oldValue (VInt (fromIntegral delta))
    putValue ref newValue
    cont (oldValue : rest)
  _ -> raiseReferenceError "Cannot modify non-reference"
runOpPostInc _ stack = error $ "Stack underflow: " ++ show stack

runModify :: Ident -> OpCodeHandler
runModify op (val : rest) =
  let go f g = case val of
                 VNum n -> cont (VNum (f n) : rest)
                 VInt n -> cont (VInt (g n) : rest)
                 v      -> do { n <- toNumber v; cont (VNum (f n) : rest) }

  in case op of
            "++" -> go (+ 1) (+ 1)
            "--" -> go (subtract 1) (subtract 1)
            _    -> raiseError $ "No such modifier: " <> op
runModify _ stack = error $ "Stack underflow: " ++ show stack

-- ref 11.4.1
runDelete :: OpCodeHandler
runDelete (val:rest)
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
runDelete stack = error $ "Stack underflow: " ++ show stack

-- ref 11.4.3
runTypeof :: OpCodeHandler
runTypeof (val:rest) =
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
runTypeof stack = error $ "Stack underflow: " ++ show stack

runStore :: OpCodeHandler
runStore (val:lhs:rest) =
  case lhs of
    VRef ref -> putValue ref val >> cont (val : rest)
    _        -> raiseReferenceError . T.pack $ show lhs ++ " is not assignable"
runStore stack = error $ "Stack underflow: " ++ show stack

-- ref 11.2.2
runNewCall :: Int -> OpCodeHandler
runNewCall n stack =
  let (args,(funcRef:rest)) = splitAt n stack
  in do
    func <- getValue funcRef
    assertFunction (T.pack $ show func) (view cstrMethod) func  -- XXX need to get the name here
    v <- newObjectFromConstructor func (reverse args)
    cont (VObj v : rest)


-- ref 11.2.3
runFunCall :: Int -> OpCodeHandler
runFunCall n stack =
  let (args,(func:rest)) = splitAt n stack
  in do
    v <- callFunction func (reverse args)
    cont (v : rest)

runLambda :: OpCodeHandler
runLambda stack = do
  (VLambda name params strict body, rest) <- pop stack
  env <- lexEnv <$> getGlobalContext
  func <- createFunction name params strict body env
  cont (func : rest)

runDiscard :: OpCodeHandler
runDiscard (_:rest) = cont rest
runDiscard stack = error $ "Stack underflow: " ++ show stack

runDup :: OpCodeHandler
runDup (a:rest) = cont (a:a:rest)
runDup stack = error $ "Stack underflow: " ++ show stack

runSwap :: OpCodeHandler
runSwap (a:b:rest) = cont (b:a:rest)
runSwap stack = error $ "Stack underflow: " ++ show stack

runRoll3 :: OpCodeHandler
runRoll3 (a:b:c:rest) = cont (b:c:a:rest)
runRoll3 stack = error $ "Stack underflow: " ++ show stack

runOpBranch ::  Int -> OpCodeHandler
runOpBranch = branch

runOpCondBranch :: (JSVal -> Bool) -> Int -> OpCodeHandler
runOpCondBranch p skip stack = do
  (val, s') <- pop stack
  if p val
  then branch skip s'
  else cont s'

runIfEq :: JSVal -> OpCodeHandler -> OpCodeHandler
runIfEq val action stack = do
  (tos, s') <- pop stack
  if tos == val
  then action s'
  else cont s'

push :: JSVal -> OpCodeHandler
push v stack = cont (v:stack)

pop :: Stack -> Runtime (JSVal, Stack)
pop (x:xs) = return (x, xs)
pop []     = raiseError "Stack underflow"

frob :: (JSVal -> JSVal) -> OpCodeHandler
frob f (x:xs) = cont (f x : xs)
frob _ [] = raiseError "Stack underflow"
