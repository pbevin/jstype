module Eval where

import Control.Monad.State
import Control.Monad.Except
import Control.Monad.Writer
import Control.Applicative
import Data.Maybe
import Data.List (intercalate)
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

runJStr :: String -> IO (Either JSError String)
runJStr = runJS ""


runJS :: String -> String -> IO (Either JSError String)
runJS sourceName input = do
  r <- runJS' sourceName input
  case r of
    ((Left err, _), _)     -> return $ Left err
    ((Right _, output), _) -> return $ Right output

runJS' :: String -> String -> IO ((Either JSError (), String), JSGlobal)
runJS' sourceName input = case parseJS' input sourceName of
  Left err -> return ((Left (show err, []), ""), JSGlobal Nothing)
  Right ast -> runJSRuntime (runProg ast)

jsEvalExpr :: String -> IO JSVal
jsEvalExpr input = do
  result <- runJSRuntime (initialCxt >>= \cxt -> runExprStmt cxt $ parseExpr input)
  case result of
    ((Left err, _), _)  -> error (show err)
    ((Right val, _), _) -> return val

runJSRuntime :: JSRuntime a -> IO ((Either JSError a, String), JSGlobal)
runJSRuntime a = runStateT (runWriterT $ runExceptT $ unJS a) (JSGlobal Nothing)

initialCxt :: JSRuntime JSCxt
initialCxt = JSCxt <$> initialEnv
                   <*> emptyEnv
                   <*> (VObj <$> getGlobalObject)

initialEnv :: JSRuntime JSEnv
initialEnv = do
  console <- newObject
  modifyRef console $ objSetProperty "log" (VNative jsConsoleLog)

  function <- newObject
  modifyRef function $ \obj -> obj { callMethod = Just funConstructor }

  object <- newObject
  modifyRef object $ objSetProperty "getOwnPropertyDescriptor" (VNative getOwnPropertyDescriptor)
  modifyRef object $ \obj -> obj { callMethod = Just objConstructor }

  error <- newObject
  modifyRef error $ \obj -> obj { callMethod = Just errConstructor }

  share $ M.fromList [ ("console", VObj console),
                       ("Function", VObj function),
                       ("Object", VObj object),
                       ("Error", VObj error) ]

emptyEnv :: JSRuntime JSEnv
emptyEnv = share M.empty


runProg :: Program -> JSRuntime ()
runProg (Program stmts) = do
  createGlobalThis
  cxt <- initialCxt
  result <- runStmts cxt stmts
  case result of
    (CTNormal, _, _) -> return ()
    (CTThrow, Just (VException (err, trace)), _) -> stackTrace (err, trace)
    _ -> liftIO $ putStrLn $ "Abnormal exit: " ++ show result

returnThrow :: Statement -> JSError -> JSRuntime StmtReturn
returnThrow s (err, trace) = return (CTThrow, Just $ VException (err, (sourceLocation s):trace), Nothing)

runStmts :: JSCxt -> [Statement] -> JSRuntime StmtReturn
runStmts _ [] = return (CTNormal, Nothing, Nothing)
runStmts cxt (s:stmts) = do
  result <- runStmt cxt s `catchError` returnThrow s
  case result of
    (CTNormal, _, _) -> runStmts cxt stmts
    _ -> return result

runStmt :: JSCxt -> Statement -> JSRuntime StmtReturn
runStmt cxt s = case s of
  ExprStmt loc e -> do
    val <- runExprStmt cxt e
    return (CTNormal, Just val, Nothing)

  VarDecl loc assignments -> do
    F.forM_ assignments $ \(x, e) -> case e of
      Nothing  -> putVar cxt x VUndef
      Just e' -> do
        v <- getValue =<< runExprStmt cxt e'
        putVar cxt x v
    return (CTNormal, Nothing, Nothing)

  For loc (For3 e1 e2 e3) stmt -> do -- ref 12.6.3
    maybeRunExprStmt cxt e1 >> keepGoing Nothing where
      keepGoing v = do
        willEval <- case e2 of
          Nothing  -> pure True
          Just e2' -> isTruthy <$> runExprStmt cxt e2'

        if not willEval
        then return (CTNormal, v, Nothing)
        else do
          sval@(stype, v', _) <- runStmt cxt stmt
          let nextVal = case v' of
                          Nothing -> v
                          Just newVal -> Just newVal
          case stype of
            CTBreak -> return (CTNormal, v, Nothing)
            CTContinue -> do
              maybeRunExprStmt cxt e3
              keepGoing nextVal
            CTNormal -> do
              maybeRunExprStmt cxt e3
              keepGoing nextVal
            _ -> return sval

  IfStatement loc predicate ifThen ifElse -> do -- ref 12.5
    v <- runExprStmt cxt predicate >>= getValue
    if toBoolean v
    then runStmt cxt ifThen
    else case ifElse of
           Nothing -> return (CTNormal, Nothing, Nothing)
           Just s  -> runStmt cxt s

  DoWhileStatement loc e s -> keepGoing Nothing True where -- ref 12.6.1
    keepGoing :: Maybe JSVal -> Bool -> JSRuntime StmtReturn
    keepGoing v False = return (CTNormal, v, Nothing)
    keepGoing v True = do
      sval@(stype, v', _) <- runStmt cxt s
      let nextVal = case v' of
                      Nothing -> v
                      Just newVal -> Just newVal
      case stype of
        CTBreak -> return (CTNormal, v, Nothing)
        CTNormal -> do
          stillIterating <- runExprStmt cxt e >>= getValue
          keepGoing v (toBoolean stillIterating)
        _ -> return sval



  Block loc stmts -> runStmts cxt stmts

  TryStatement _loc block catch finally -> do -- ref 12.14
    b@(btype, bval, _) <- runStmt cxt block
    if btype /= CTThrow
    then return b
    else runCatch cxt catch (fromJust bval)

  ThrowStatement loc e -> do
    exc <- runExprStmt cxt e >>= getValue
    return (CTThrow, Just exc, Nothing)

  BreakStatement loc -> return (CTBreak, Nothing, Nothing)
  Return loc Nothing -> return (CTReturn, Just VUndef, Nothing)
  Return loc (Just e) -> do
    val <- runExprStmt cxt e
    return (CTReturn, Just val, Nothing)

  EmptyStatement loc -> return (CTNormal, Nothing, Nothing)

  _ -> error ("Unimplemented stmt: " ++ show s)

maybeRunExprStmt :: JSCxt -> Maybe Expr -> JSRuntime ()
maybeRunExprStmt _ Nothing  = return ()
maybeRunExprStmt cxt (Just e) = void (runExprStmt cxt e)


-- ref 12.14
runCatch :: JSCxt -> Maybe Catch -> JSVal -> JSRuntime StmtReturn
runCatch _ Nothing _ = return (CTNormal, Nothing, Nothing)
runCatch cxt (Just (Catch _loc var stmt)) exc = do
  -- XXX create new environment
  putVar cxt var exc
  runStmt cxt stmt





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
      _ -> raiseError $ "Cannot read property '" ++ x ++ "' of " ++ show lval

  FunCall f args -> do  -- ref 11.2.3
    ref <- runExprStmt cxt f
    func <- getValue ref
    argList <- evalArguments cxt args
    let thisValue = computeThisValue cxt ref
    objCall cxt func thisValue argList

  Assign lhs op e -> do
    lref <- runExprStmt cxt lhs
    rref <- runExprStmt cxt e
    updateRef (assignOp op) lref rref

  BinOp "&&" e1 e2 -> do
    v1 <- runExprStmt cxt e1 >>= getValue
    if toBoolean v1
    then runExprStmt cxt e2 >>= getValue
    else return $ VBool False

  BinOp op e1 e2 -> do
    v1 <- runExprStmt cxt e1 >>= getValue
    v2 <- runExprStmt cxt e2 >>= getValue
    evalBinOp op v1 v2

  UnOp "typeof" e -> do -- ref 11.4.3
    val <- runExprStmt cxt e >>= getValue
    return $ VStr $ case typeof val of
      TypeUndefined -> "undefined"
      TypeNull      -> "null"
      TypeBoolean   -> "boolean"
      TypeNumber    -> "number"
      TypeString    -> "string"
      TypeObject    -> "object"  -- or "function"

  UnOp "+" e -> do
    val <- runExprStmt cxt e >>= getValue
    return $ VNum $ toNumber val

  UnOp op e -> do -- ref 11.4.{4,5}
    lhs <- runExprStmt cxt e
    lval <- getValue lhs
    let newVal = postfixUpdate op lval
    putValue lhs newVal
    return newVal

  PostOp op e -> do -- ref 11.3
    lhs <- runExprStmt cxt e
    lval <- getValue lhs
    putValue lhs (postfixUpdate op lval)
    return lval

  NewExpr f args -> do
    fun <- runExprStmt cxt f >>= getValue
    argList <- evalArguments cxt args
    liftM VObj (newObjectFromConstructor cxt fun argList)

  FunDef (Just name) params body -> do
    fun <- createFunction params body cxt
    putVar cxt name fun
    return fun

  FunDef Nothing params body -> createFunction params body cxt

  ObjectLiteral nameValueList -> makeObjectLiteral cxt nameValueList

  _              -> error ("Unimplemented expr: " ++ show expr)

evalArguments :: JSCxt -> [Expr] -> JSRuntime [JSVal]
evalArguments cxt = mapM (runExprStmt cxt >=> getValue)

computeThisValue :: JSCxt -> JSVal -> JSVal
computeThisValue cxt v = case v of
  VRef ref ->
    if isPropertyReference ref
    then getBase ref
    else thisBinding cxt

  _ -> thisBinding cxt


putVar :: JSCxt -> Ident -> JSVal -> JSRuntime ()
putVar (JSCxt envref _ _) x v = modifyRef envref $ M.insert x v

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

-- ref 15.2.3.3
getOwnPropertyDescriptor :: JSVal -> [JSVal] -> JSRuntime JSVal
getOwnPropertyDescriptor _this xs = do
  let [objVal, propVal] = xs

  obj <- getValue objVal
  val <- getValue (VRef $ JSRef obj (toString propVal) False)

  result <- newObject
  modifyRef result $ objSetProperty "value" val
  modifyRef result $ objSetProperty "writable" (VBool True)
  modifyRef result $ objSetProperty "enumerable" (VBool False)
  modifyRef result $ objSetProperty "configurable" (VBool True)

  return $ VObj result

-- ref B.2.1, incomplete
objEscape :: JSFunction
objEscape _this [] = return $ VUndef
objEscape _this (x:xs) = return $ x


showVal :: JSVal -> String
showVal (VStr s) = s
showVal (VNum (JSNum n)) = show (round n :: Integer)
showVal VUndef = "(undefined)"
showVal other = show other

postfixUpdate :: String -> JSVal -> JSVal
postfixUpdate "++" (VNum v) = VNum (v+1)
postfixUpdate "--" (VNum v) = VNum (v-1)
postfixUpdate op v = error $ "No such postfix op " ++ op ++ " on " ++ show v

evalBinOp :: String -> JSVal -> JSVal -> JSRuntime JSVal
evalBinOp "===" x y = return $ VBool $ tripleEquals x y
evalBinOp "!==" x y = return $ VBool $ not $ tripleEquals x y
evalBinOp "+" (VStr a) (VStr b) = return $ VStr (a ++ b)
evalBinOp op x@(VNum v1) y@(VNum v2) = case op of
  "+" -> return $ VNum $ v1 + v2
  "-" -> return $ VNum $ v1 - v2
  "*" -> return $ VNum $ v1 * v2
  "/" -> return $ VNum $ v1 / v2
  "<" -> return $ VBool $ v1 < v2
  ">" -> return $ VBool $ v1 > v2
  _ -> raiseError $ "No binop " ++ op ++ " on " ++ show (v1, v2)
evalBinOp op v1 v2 = raiseError $ "No binop " ++ op ++ " on " ++ show (v1, v2)


-- ref 11.9.6, incomplete
tripleEquals :: JSVal -> JSVal -> Bool
tripleEquals x y
  | typeof x /= typeof y        = False
  | typeof x == TypeUndefined   = True
  | typeof x == TypeNull        = True
  | typeof x == TypeNumber      = (x == y)
  | typeof x == TypeString      = (x == y)
  | typeof x == TypeBoolean     = (x == y)
  | typeof x == TypeObject      = (x == y)
  | typeof x == TypeFunction    = (x == y)
  | otherwise = error $ "Can't === " ++ show x ++ " and " ++ show y



-------------------------------------------------

createFunction :: [Ident] -> [Statement] -> JSCxt -> JSRuntime JSVal
createFunction paramList body cxt = do
  objref <- newObject
  modifyRef objref $
    \obj -> obj { objClass = "Function",
                  callMethod = Just (funcCall cxt paramList body) }
  return $ VObj objref

setClass :: String -> Shared JSObj -> JSRuntime (Shared JSObj)
setClass cls objRef = modifyRef' objRef $ \obj -> obj { objClass = cls }

addOwnProperty :: String -> JSVal -> Shared JSObj -> JSRuntime (Shared JSObj)
addOwnProperty name val objRef = modifyRef' objRef $ objSetProperty name val

createError :: JSVal -> JSRuntime JSVal
createError text =
  VObj <$> (newObject >>= setClass "Error"
                      >>= addOwnProperty "message" text)

-- ref 13.2.1, incomplete
funcCall :: JSCxt -> [Ident] -> [Statement] -> JSVal -> [JSVal] -> JSRuntime JSVal
funcCall cxt paramList body this args =
  let makeRef name = JSRef (VCxt cxt) name False
      refs = map makeRef paramList
      newCxt = cxt { thisBinding = this }
  in do
    zipWithM_ putEnvironmentRecord refs args
    result <- runStmts newCxt body
    case result of
      (CTReturn, Just v, _)  -> return v
      (CTThrow, Just (VException (err, trace)), _) -> throwError (err, trace)
      _ -> return VUndef

-- ref 13.2.2, incomplete
newObjectFromConstructor :: JSCxt -> JSVal -> [JSVal] -> JSRuntime (Shared JSObj)
newObjectFromConstructor cxt fun@(VObj funref) args = do
  obj <- newObject
  f <- deref funref
  prototype <- objGetProperty "prototype" f
  modifyRef obj $ objSetProperty "prototype" $ fromMaybe VUndef prototype
  objCall cxt fun (VObj obj) args
  return obj

objConstructor :: JSVal -> [JSVal] -> JSRuntime JSVal
objConstructor _this _args = VObj <$> newObject

errConstructor :: JSVal -> [JSVal] -> JSRuntime JSVal
errConstructor this args =
  let text = head args
  in case this of
    VObj obj -> do
      x <- return obj >>= setClass "Error"
                      >>= addOwnProperty "message" text
      return (VObj x)


funConstructor :: JSVal -> [JSVal] -> JSRuntime JSVal
funConstructor this [arg] =
  let body = toString arg
      params = []
      Program stmts = simpleParse body
  in createFunction params stmts =<< JSCxt <$> initialEnv
                                           <*> emptyEnv
                                           <*> (VObj <$> getGlobalObject)
funConstructor this xs = error $ "Can't cstr Function with " ++ show xs

toString (VStr s) = s

objCall :: JSCxt -> JSVal -> JSVal -> [JSVal] -> JSRuntime JSVal
objCall cxt func this args = case func of
  VNative f -> f this args
  VObj objref -> deref objref >>= \obj -> case callMethod obj of
    Nothing -> error "Can't call function: no callMethod"
    Just method -> method this args
  _ -> error $ "Can't call " ++ show func


stackTrace :: JSError -> JSRuntime ()
stackTrace (err, trace) = liftIO $ do
  putStrLn $ "Error: " ++ err
  mapM_ print (reverse trace)

createGlobalThis :: JSRuntime ()
createGlobalThis = do
  this <- newObject
  modifyRef this $ objSetProperty "escape" (VNative objEscape)
  put $ JSGlobal (Just this)

getGlobalObject :: JSRuntime (Shared JSObj)
getGlobalObject = do
  global <- get
  return $ fromJust $ globalObject global

makeObjectLiteral :: JSCxt -> [(PropertyName, Expr)] -> JSRuntime JSVal
makeObjectLiteral cxt nameValueList =
  let nameOf :: PropertyName -> String
      nameOf (IdentProp ident) = ident
      nameOf (StringProp str)  = str
      nameOf (NumProp n)       = show n
      addProp :: Shared JSObj -> (PropertyName, Expr) -> JSRuntime (Shared JSObj)
      addProp obj (propName, propValue) = do
        val <- runExprStmt cxt propValue >>= getValue
        return obj >>= addOwnProperty (nameOf propName) val
  in do
    obj <- newObject
    VObj <$> foldM addProp obj nameValueList
