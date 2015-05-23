{-# LANGUAGE LambdaCase #-}

module Eval where

import Control.Monad.State
import Control.Monad.Except
import Control.Monad.Writer
import Control.Applicative
import Data.Maybe
import Data.List (intercalate)
import Data.Bits
import Data.Fixed
import qualified Data.Foldable as F
import qualified Data.Map as M
import Text.Show.Functions
import Parse
import Expr
import Runtime.Types
import Runtime.Object
import Runtime.Reference
import Runtime.Conversion
import Runtime.Operations
import Debug.Trace

runJStr :: String -> IO (Either JSError String)
runJStr = runJS ""


evalJS :: String -> String -> IO (Either JSError (Maybe JSVal))
evalJS sourceName input = do
  runJS' sourceName input >>= \case
    ((Left err, _), _) -> return $ Left err
    ((Right v, _), _)  -> return $ Right v

runJS :: String -> String -> IO (Either JSError String)
runJS sourceName input = do
  r <- runJS' sourceName input
  case r of
    ((Left err, _), _)     -> return $ Left err
    ((Right _, output), _) -> return $ Right output

runJS' :: String -> String -> IO ((Either JSError (Maybe JSVal), String), JSGlobal)
runJS' sourceName input = case parseJS' input sourceName of
  Left err -> return ((Left ("SyntaxError", VStr $ show err, []), ""), JSGlobal Nothing)
  Right ast -> runJSRuntime (runProg ast)

jsEvalExpr :: String -> IO JSVal
jsEvalExpr input = do
  result <- runtime $ do
    createGlobalThis
    cxt <- initialCxt
    runExprStmt cxt (parseExpr input) >>= getValue
  case result of
    Left err  -> error (show err)
    Right val -> return val

runtime :: JSRuntime a -> IO (Either JSError a)
runtime p = do
  result <- runJSRuntime p
  return $ fst (fst result)


runJSRuntime :: JSRuntime a -> IO ((Either JSError a, String), JSGlobal)
runJSRuntime a = runStateT (runWriterT $ runExceptT $ unJS a) (JSGlobal Nothing)

initialCxt :: JSRuntime JSCxt
initialCxt = JSCxt <$> initialEnv
                   <*> emptyEnv
                   <*> (VObj <$> getGlobalObject)

initialEnv :: JSRuntime JSEnv
initialEnv = do
  global <- getGlobalObject
  share $ LexEnv (ObjEnvRec global) Nothing


emptyEnv :: JSRuntime JSEnv
emptyEnv = do
  m <- share M.empty
  share $ LexEnv (DeclEnvRec m) Nothing

runProg :: Program -> JSRuntime (Maybe JSVal)
runProg (Program strictness stmts) = do
  createGlobalThis
  cxt <- initialCxt
  result <- runStmts cxt stmts
  case result of
    (CTNormal, v, _) -> return v
    (CTThrow, Just (VException exc@(excType, err, trace)), _) -> do
      msg <- callToString cxt err
      printStackTrace exc
      throwError (excType, msg, trace)
    (CTThrow, Just v, _) -> do
      v' <- callToString cxt v
      throwError ("Error", v', [])
    _ -> do
      liftIO $ putStrLn $ "Abnormal exit: " ++ show result
      return Nothing

callToString :: JSCxt -> JSVal -> JSRuntime JSVal
callToString _ (VStr str) = return (VStr str)
callToString cxt v = do
  obj <- toObject cxt v
  ref <- memberGet obj "toString"
  funCall cxt ref []




returnThrow :: Statement -> JSError -> JSRuntime StmtReturn
returnThrow s (excType, err, trace) =
 let exc = VException (excType, err, sourceLocation s : trace)
 in return (CTThrow, Just exc, Nothing)

runStmts :: JSCxt -> [Statement] -> JSRuntime StmtReturn
runStmts = runStmts' (CTNormal, Nothing, Nothing) where
  runStmts' emptyResult _ [] = return emptyResult
  runStmts' _ cxt (s:stmts) = do
    result <- runStmt cxt s `catchError` returnThrow s
    case result of
      (CTNormal, _, _) -> runStmts' result cxt stmts
      _ -> return result

runStmt :: JSCxt -> Statement -> JSRuntime StmtReturn
runStmt cxt s = case s of
  ExprStmt loc e -> do
    val <- runExprStmt cxt e >>= getValue
    return (CTNormal, Just val, Nothing)

  VarDecl loc assignments -> do
    F.forM_ assignments $ \(x, e) -> case e of
      Nothing  -> putVar cxt x VUndef
      Just e' -> do
        v <- getValue =<< runExprStmt cxt e'
        putVar cxt x v
    return (CTNormal, Nothing, Nothing)

  LabelledStatement loc label stmt -> runStmt cxt stmt

  IfStatement loc predicate ifThen ifElse -> do -- ref 12.5
    v <- runExprStmt cxt predicate >>= getValue
    if toBoolean v
    then runStmt cxt ifThen
    else case ifElse of
           Nothing -> return (CTNormal, Nothing, Nothing)
           Just s  -> runStmt cxt s

  DoWhileStatement loc e s -> -- ref 12.6.1
    runStmt cxt $ transformDoWhileToWhile loc e s

  WhileStatement loc e stmt -> keepGoing Nothing where -- ref 12.6.2
    keepGoing :: Maybe JSVal -> JSRuntime StmtReturn
    keepGoing v = do
      willEval <- isTruthy <$> (runExprStmt cxt e >>= getValue)

      if not willEval
      then return (CTNormal, v, Nothing)
      else do
        sval@(stype, v', _) <- runStmt cxt stmt
        let nextVal = case v' of
                        Nothing -> v
                        Just newVal -> Just newVal
        case stype of
          CTBreak -> return (CTNormal, v, Nothing)
          CTContinue -> keepGoing nextVal
          CTNormal -> keepGoing nextVal
          _ -> return sval

  For loc (For3 e1 e2 e3) stmt -> -- ref 12.6.3
    runStmt cxt $ transformFor3ToWhile loc e1 e2 e3 stmt

  For loc (For3Var x e1 e2 e3) stmt -> -- ref 12.6.3
    runStmt cxt $ transformFor3VarToWhile loc x e1 e2 e3 stmt

  Block loc stmts -> runStmts cxt stmts

  TryStatement _loc block catch finally -> do -- ref 12.14
    b@(btype, bval, _) <- runStmt cxt block
    if btype /= CTThrow
    then return b
    else runCatch cxt catch (fromJust bval)

  ThrowStatement loc e -> do
    exc <- runExprStmt cxt e >>= getValue
    return (CTThrow, Just exc, Nothing)

  BreakStatement loc label -> return (CTBreak, Nothing, label)
  ContinueStatement loc label -> return (CTBreak, Nothing, label)
  Return loc Nothing -> return (CTReturn, Just VUndef, Nothing)
  Return loc (Just e) -> do
    val <- runExprStmt cxt e >>= getValue
    return (CTReturn, Just val, Nothing)

  EmptyStatement loc -> return (CTNormal, Nothing, Nothing)

  _ -> error ("Unimplemented stmt: " ++ show s)

-- |
-- Turn "for (e1; e2; e3) { s }" into
-- "e1; while (e2) { s; e3 }"
-- with sensible defaults for missing statements
transformFor3ToWhile :: SrcLoc -> Maybe Expr -> Maybe Expr -> Maybe Expr -> Statement -> Statement
transformFor3ToWhile loc e1 e2 e3 stmt =
  let e = fromMaybe (Boolean True) e2
      s1 = maybe (EmptyStatement loc) (ExprStmt loc) e1
      s2 = [ stmt, maybe (EmptyStatement loc) (ExprStmt loc) e3 ]
  in Block loc [ s1, WhileStatement loc e (Block loc s2) ]

-- |
-- Turn "for (var x = e1; e2; e3) { s }" into
-- "var x = e1; while (e2) { s; e3 }"
-- with sensible defaults for missing statements
transformFor3VarToWhile :: SrcLoc -> Ident -> Expr -> Maybe Expr -> Maybe Expr -> Statement -> Statement
transformFor3VarToWhile loc x e1 e2 e3 stmt =
  let e = fromMaybe (Boolean True) e2
      s1 = VarDecl loc [(x, Just e1)]
      s2 = [ stmt, maybe (EmptyStatement loc) (ExprStmt loc) e3 ]
  in Block loc [ s1, WhileStatement loc e (Block loc s2) ]

-- |
-- Turn "do { s } while (e)" into
-- "while (true) { s; if (!e) break; }"
transformDoWhileToWhile :: SrcLoc -> Expr -> Statement -> Statement
transformDoWhileToWhile loc e s =
  let esc = IfStatement loc (UnOp "!" e)
              (BreakStatement loc Nothing)
              Nothing
  in WhileStatement loc (Boolean True) $ Block loc [ s, esc ]


-- ref 12.14
runCatch :: JSCxt -> Maybe Catch -> JSVal -> JSRuntime StmtReturn
runCatch _ Nothing _ = return (CTNormal, Nothing, Nothing)
runCatch cxt (Just (Catch _loc var stmt)) exc = do
  -- XXX create new environment
  putVar cxt var exc
  runStmt cxt stmt

maybeValList :: JSCxt -> [Maybe Expr] -> JSRuntime [Maybe JSVal]
maybeValList cxt = mapM (evalOne cxt)
  where
    evalOne cxt v = case v of
      Nothing -> return Nothing
      Just e  -> Just <$> (runExprStmt cxt e >>= getValue)

memberGet :: JSVal -> String -> JSRuntime JSVal
memberGet lval prop =
  case lval of
    VObj _ -> return $ VRef (JSRef lval prop NotStrict)
    _ -> raiseError $ "Cannot read property '" ++ prop ++ "' of " ++ show lval

funCall :: JSCxt -> JSVal -> [JSVal] -> JSRuntime JSVal
funCall cxt ref argList = do
  func <- getValue ref
  if typeof func == TypeUndefined
  then raiseError $ "Function " ++ getReferencedName (unwrapRef ref) ++ " is undefined"
  else let thisValue = computeThisValue cxt ref
       in objCall cxt func thisValue argList


readVar :: JSCxt -> Ident -> Strictness -> JSRuntime JSVal
readVar cxt name strictness =
  VRef <$> getIdentifierReference (Just $ lexEnv cxt) name strictness

runExprStmt :: JSCxt -> Expr -> JSRuntime JSVal
runExprStmt cxt expr = case expr of
  Num n              -> return $ VNum n
  Str s              -> return $ VStr s
  Boolean b          -> return $ VBool b
  LiteralNull        -> return VNull
  ReadVar name       -> readVar cxt name NotStrict
  ReadVarStrict name -> readVar cxt name Strict
  This               -> return $ thisBinding cxt
  ArrayLiteral vals  -> evalArrayLiteral cxt vals

  MemberDot e x -> runExprStmt cxt (MemberGet e (Str x)) -- ref 11.2.1

  MemberGet e x -> do -- ref 11.2.1
    lval <- runExprStmt cxt e >>= getValue
    prop <- runExprStmt cxt x >>= getValue >>= toString
    memberGet lval prop

  FunCall f args -> do  -- ref 11.2.3
    ref <- runExprStmt cxt f
    argList <- evalArguments cxt args
    funCall cxt ref argList

  Assign lhs op e -> do
    lref <- runExprStmt cxt lhs
    rref <- runExprStmt cxt e
    case op of
      "=" -> assignRef lref rref
      _   -> updateRef (init op) lref rref

  BinOp "&&" e1 e2 -> do
    v1 <- runExprStmt cxt e1 >>= getValue
    if toBoolean v1
    then runExprStmt cxt e2 >>= getValue
    else return v1

  BinOp "||" e1 e2 -> do
    v1 <- runExprStmt cxt e1 >>= getValue
    if toBoolean v1
    then return v1
    else runExprStmt cxt e2 >>= getValue

  BinOp op e1 e2 -> do
    v1 <- runExprStmt cxt e1 >>= getValue
    v2 <- runExprStmt cxt e2 >>= getValue
    evalBinOp op v1 v2

  UnOp "typeof" e -> do -- ref 11.4.3
    val <- runExprStmt cxt e
    if isReference val && isUnresolvableReference (unwrapRef val)
    then return $ VStr "undefined"
    else do
      resolved <- getValue val
      return $ VStr $ case typeof resolved of
        TypeUndefined -> "undefined"
        TypeNull      -> "null"
        TypeBoolean   -> "boolean"
        TypeNumber    -> "number"
        TypeString    -> "string"
        TypeObject    -> "object"  -- or "function"
        _ -> showVal resolved


  UnOp op e -> -- ref 11.4
    let f = case op of
              "++"   -> modifyingOp (+ 1) (+ 1)
              "--"   -> modifyingOp (subtract 1) (subtract 1)
              "+"    -> purePrefix unaryPlus
              "-"    -> purePrefix unaryMinus
              "!"    -> purePrefix unaryNot
              "~"    -> purePrefix unaryBitwiseNot
              "void" -> purePrefix (return . const VUndef)
              _    -> const $ const $ raiseError $ "Prefix not implemented: " ++ op
    in f cxt e

  PostOp op e -> -- ref 11.3
    let f = case op of
              "++" -> modifyingOp (+1) id
              "--" -> modifyingOp (subtract 1) id
    in f cxt e

  NewExpr f args -> do
    fun <- runExprStmt cxt f
    argList <- evalArguments cxt args
    liftM VObj (newObjectFromConstructor cxt fun argList)

  FunDef (Just name) params strictness body -> do
    fun <- createFunction params strictness body cxt
    putVar cxt name fun
    return fun

  FunDef Nothing params strictness body -> createFunction params strictness body cxt

  ObjectLiteral nameValueList -> makeObjectLiteral cxt nameValueList

  _              -> error ("Unimplemented expr: " ++ show expr)

evalArrayLiteral :: JSCxt -> [Maybe Expr] -> JSRuntime JSVal
evalArrayLiteral cxt vals = createArray =<< mapM evalMaybe vals
  where evalMaybe Nothing = return Nothing
        evalMaybe (Just e) = Just <$> (runExprStmt cxt e >>= getValue)

evalArguments :: JSCxt -> [Expr] -> JSRuntime [JSVal]
evalArguments cxt = mapM (runExprStmt cxt >=> getValue)

computeThisValue :: JSCxt -> JSVal -> JSVal
computeThisValue cxt v = case v of
  VRef ref ->
    if isPropertyReference ref
    then getBase ref
    else thisBinding cxt

  _ -> thisBinding cxt

modifyingOp :: (JSNum->JSNum) -> (JSNum->JSNum) -> JSCxt -> Expr -> JSRuntime JSVal
modifyingOp op returnOp cxt e = do
  lhs <- runExprStmt cxt e
  case lhs of
    VRef ref -> do
      lval <- getValue lhs
      val <- toNumber lval
      let newVal = VNum $ op val
          retVal = VNum $ returnOp val
      putValue ref newVal
      return retVal
    _ -> raiseError $ "ReferenceError: " ++ show e ++ " is not assignable"

purePrefix :: (JSVal -> JSRuntime JSVal) -> JSCxt -> Expr -> JSRuntime JSVal
purePrefix f cxt e = runExprStmt cxt e >>= getValue >>= f

putVar :: JSCxt -> Ident -> JSVal -> JSRuntime ()
putVar (JSCxt envref _ _) x v = putValue ref v where
 ref = JSRef (VEnv envref) x NotStrict


-- ref 10.2.2.1
getIdentifierReference :: Maybe JSEnv -> Ident -> Strictness -> JSRuntime JSRef
getIdentifierReference Nothing name strict = return $ JSRef VUndef name strict
getIdentifierReference (Just lexRef) name strict = do
  lex <- deref lexRef
  bound <- hasBinding name (envRec lex)
  if bound
  then return $ JSRef (VEnv lexRef) name NotStrict
  else do
    getIdentifierReference (outer lex) name strict


-- ref 11.13.1
assignRef :: JSVal -> JSVal -> JSRuntime JSVal
assignRef lref rref =
  case lref of
    VRef ref -> do
      rval <- getValue rref
      putValue ref rval
      return rval
    _ -> raiseError $ "ReferenceError: " ++ show lref ++ " is not assignable"



updateRef :: String -> JSVal -> JSVal -> JSRuntime JSVal
updateRef op lref rref =
  case lref of
    VRef ref -> do
      lval   <- getValue lref
      rval   <- getValue rref
      newVal <- evalBinOp op lval rval
      putValue ref newVal
      return newVal
    _ -> raiseError $ "ReferenceError: " ++ show lref ++ " is not assignable"

isTruthy :: JSVal -> Bool
isTruthy (VNum 0)      = False
isTruthy VUndef        = False
isTruthy (VBool False) = False
isTruthy (VStr "")     = False
isTruthy _             = True


jsConsoleLog :: JSVal -> [JSVal] -> JSRuntime JSVal
jsConsoleLog _this xs = tell (unwords (map showVal xs) ++ "\n") >> return VUndef

-- ref 15.2.3.3
getOwnPropertyDescriptor :: JSVal -> [JSVal] -> JSRuntime JSVal
getOwnPropertyDescriptor _this xs = do
  let [objVal, propVal] = xs

  obj <- getValue objVal
  str <- toString propVal
  val <- getValue (VRef $ JSRef obj str NotStrict)

  result <- newObject
  modifyRef result $ objSetProperty "value" val
  modifyRef result $ objSetProperty "writable" (VBool True)
  modifyRef result $ objSetProperty "enumerable" (VBool False)
  modifyRef result $ objSetProperty "configurable" (VBool True)

  return $ VObj result

-- ref B.2.1, incomplete
objEscape :: JSFunction
objEscape _this args = case args of
  [] -> return VUndef
  (x:xs) -> return x

-- ref 15.1.2.1
objEval :: JSFunction
objEval _this args = case args of
  [] -> return VUndef
  (prog:_) -> do
    text <- toString prog
    case parseJS' text "(eval)" of
      Left err -> raiseError $ "SyntaxError: " ++ show err
      Right (Program strictness stmts) -> do
        cxt <- JSCxt <$> initialEnv
                     <*> emptyEnv
                     <*> (VObj <$> getGlobalObject)

        (stype, sval, _) <- runStmts cxt stmts
        case stype of
          CTNormal -> return $ fromMaybe VUndef sval
          CTThrow ->
            let Just (VException err) = sval
            in throwError err

evalBinOp :: String -> JSVal -> JSVal -> JSRuntime JSVal
evalBinOp op = case op of
  "==="        -> tripleEquals
  "!=="        -> invert tripleEquals
  "=="         -> doubleEquals
  "!="         -> invert doubleEquals
  "instanceof" -> jsInstanceOf
  "+"          -> jsAdd
  "-"          -> numberOp (-)
  "*"          -> numberOp (*)
  "/"          -> numberOp (/)
  "%"          -> numberOp $ mod'
  "<"          -> lessThan                -- ref 11.8.1
  ">"          -> flip (lessThan)         -- ref 11.8.2
  "<="         -> flip (invert lessThan)  -- ref 11.8.3
  ">="         -> invert lessThan         -- ref 11.8.4
  "&"          -> bitwise (.&.)           -- ref 11.10
  "|"          -> bitwise (.|.)           -- ref 11.10
  "^"          -> bitwise xor             -- ref 11.10
  "<<"         -> bitshift shiftL
  ">>"         -> bitshift shiftR
  ">>>"        -> bitshift shiftR
  ","          -> commaOperator
  _            -> noSuchBinop op
  where invert op x y = unaryNot =<< op x y

noSuchBinop :: String -> JSVal -> JSVal -> JSRuntime JSVal
noSuchBinop op a b = raiseError $
  "No binop `" ++ op ++ "' on " ++ show (a, b)




-------------------------------------------------

createFunction :: [Ident] -> Strictness -> [Statement] -> JSCxt -> JSRuntime JSVal
createFunction paramList strict body cxt = do
  objref <- newObject
  modifyRef objref $
    \obj -> obj { objClass = "Function",
                  callMethod = Just (funcCall cxt paramList body) }
  return $ VObj objref

createError :: JSVal -> JSRuntime JSVal
createError text =
  VObj <$> (newObject >>= setClass "Error"
                      >>= addOwnProperty "message" text)

-- ref 13.2.1, incomplete
funcCall :: JSCxt -> [Ident] -> [Statement] -> JSVal -> [JSVal] -> JSRuntime JSVal
funcCall cxt paramList body this args =
  let makeRef env name = JSRef (VEnv env) name NotStrict
      newCxt env = cxt { thisBinding = this, lexEnv = env }
      addToNewEnv :: JSEnv -> Ident -> JSVal -> JSRuntime ()
      addToNewEnv env x v = putEnvironmentRecord (makeRef env x) v
  in do
    env <- newEnv (lexEnv cxt)
    zipWithM_ (addToNewEnv env) paramList args
    result <- runStmts (newCxt env) body
    case result of
      (CTReturn, Just v, _) -> return v
      (CTThrow, Just (VException v), _) -> throwError v
      (CTThrow, Just v, _)  -> throwError ("", v, [])
      _ -> return VUndef

-- ref 13.2.2, incomplete
newObjectFromConstructor :: JSCxt -> JSVal -> [JSVal] -> JSRuntime (Shared JSObj)
newObjectFromConstructor cxt fun args = case fun of
  VRef (JSRef _ name _) -> create name =<< getValue fun
  _                     -> create (show fun) fun
  where
    create :: String -> JSVal -> JSRuntime (Shared JSObj)
    create name f = case f of
      VObj funref -> do
        obj <- newObject
        f <- deref funref
        prototype <- objGetProperty "prototype" f
        modifyRef obj $ objSetProperty "prototype" $ fromMaybe VUndef prototype
        objCall cxt (VObj funref) (VObj obj) args
        return obj
      _ -> raiseError $ "Can't invoke constructor " ++ name

objConstructor :: JSVal -> [JSVal] -> JSRuntime JSVal
objConstructor _this _args = VObj <$> newObject

numConstructor :: JSVal -> [JSVal] -> JSRuntime JSVal
numConstructor this args = do
  num <- VNum <$> if null args
                  then return 0
                  else toNumber (head args)
  case this of
    VObj obj -> do
      VObj <$> (setClass "Number" obj >>= setPrimitiveValue num)
    _ -> raiseError $ "numConstructor called with this = " ++ show this

boolConstructor :: JSVal -> [JSVal] -> JSRuntime JSVal
boolConstructor this args = do
  let val = VBool $ if null args
                    then False
                    else toBoolean (head args)
  case this of
    VObj obj -> do
      VObj <$> (setClass "Boolean" obj >>= setPrimitiveValue val)
    _ -> raiseError $ "boolConstructor called with this = " ++ show this

stringConstructor :: JSFunction
stringConstructor this args =
  let val = VBool $ if null args
                    then False
                    else toBoolean (head args)
  in case this of
    VObj obj -> do
      VObj <$> (setClass "String" obj >>= setPrimitiveValue val)
    _ -> raiseError $ "boolConstructor called with this = " ++ show this

arrayConstructor :: JSFunction
arrayConstructor this args =
  let len = VNum $ fromIntegral $ length args
  in case this of
    VObj obj -> do
      VObj <$> (setClass "Array" obj >>= addOwnProperty "length" len)
    _ -> raiseError $ "boolConstructor called with this = " ++ show this

createArray :: [Maybe JSVal] -> JSRuntime JSVal
createArray vals =
  let len = VNum $ fromIntegral $ length vals
  in do
    obj <- newObject
    VObj <$> (setClass "Array" obj >>= addOwnProperty "length" len)


errConstructor :: JSVal -> [JSVal] -> JSRuntime JSVal
errConstructor this args =
  let text = head args
  in case this of
    VObj obj -> do
      setClass "Error" obj >>= addOwnProperty "message" text
                           >>= addOwnProperty "toString" (VNative errorToString)
                           >>= return . VObj

errorToString :: JSFunction
errorToString (VObj this) _args = do
  obj <- deref this
  msg <- objGetProperty "message" obj
  return $ VStr $ objClass obj ++ ": " ++ showVal (fromMaybe VUndef msg)

funConstructor :: JSVal -> [JSVal] -> JSRuntime JSVal
funConstructor this [arg] = do
  body <- toString arg
  let Program strictness stmts = simpleParse body
  createFunction [] strictness stmts =<< JSCxt <$> initialEnv
                                               <*> emptyEnv
                                               <*> (VObj <$> getGlobalObject)
funConstructor this xs = error $ "Can't cstr Function with " ++ show xs

objCall :: JSCxt -> JSVal -> JSVal -> [JSVal] -> JSRuntime JSVal
objCall cxt func this args = case func of
  VNative f -> f this args
  VObj objref -> deref objref >>= \obj -> case callMethod obj of
    Nothing -> raiseError "Can't call function: no callMethod"
    Just method -> method this args
  VUndef -> raiseError "Undefined function"
  _ -> raiseError $ "Can't call " ++ show func


stackTrace :: JSError -> String
stackTrace (excType, err, trace) = unlines $ show err : map show (reverse trace)


printStackTrace :: JSError -> JSRuntime ()
printStackTrace = liftIO . putStrLn . stackTrace

createGlobalThis :: JSRuntime ()
createGlobalThis = do
  console <- newObject
  modifyRef console $ objSetProperty "log" (VNative jsConsoleLog)

  function <- newObject
  modifyRef function $ \obj -> obj { callMethod = Just funConstructor }

  object <- newObject
  modifyRef object $ objSetProperty "getOwnPropertyDescriptor" (VNative getOwnPropertyDescriptor)
  modifyRef object $ \obj -> obj { callMethod = Just objConstructor }

  number <- newObject >>= addOwnProperty "NaN" (VNum jsNaN)
                      >>= addOwnProperty "isNaN" (VNative objIsNaN)
                      >>= setCallMethod numConstructor

  boolean <- newObject >>= setCallMethod boolConstructor

  string <- newObject >>= setCallMethod stringConstructor

  array <- newObject >>= setCallMethod arrayConstructor

  error <- newObject
  modifyRef error $ \obj -> obj { callMethod = Just errConstructor }

  math <- newObject >>= addOwnProperty "PI" (VNum $ JSNum (pi :: Double))
                    >>= addOwnProperty "E" (VNum $ JSNum (exp 1 :: Double))
                    >>= addOwnProperty "LN2" (VNum $ JSNum (log 2 :: Double))
                    >>= addOwnProperty "LN10" (VNum $ JSNum (log 10 :: Double))
                    >>= addOwnProperty "LOG10E" (VNum $ JSNum $ 1 / (log 10 :: Double))
                    >>= addOwnProperty "LOG2E" (VNum $ JSNum $ 1 / (log 2 :: Double))
                    >>= addOwnProperty "abs" (VNative $ mathFunc abs)
                    >>= addOwnProperty "log" (VNative $ mathFunc log)
                    >>= addOwnProperty "exp" (VNative $ mathFunc exp)
                    >>= addOwnProperty "sin" (VNative $ mathFunc sin)
                    >>= addOwnProperty "cos" (VNative $ mathFunc cos)
                    >>= addOwnProperty "tan" (VNative $ mathFunc tan)
                    >>= addOwnProperty "asin" (VNative $ mathFunc asin)
                    >>= addOwnProperty "acos" (VNative $ mathFunc acos)
                    >>= addOwnProperty "atan" (VNative $ mathFunc atan)
                    >>= addOwnProperty "sqrt" (VNative $ mathFunc sqrt)
                    >>= addOwnProperty "ceil" (VNative $ mathFunc $ fromIntegral . ceiling)
                    >>= addOwnProperty "round" (VNative $ mathFunc $ fromIntegral . round)
                    >>= addOwnProperty "floor" (VNative $ mathFunc $ fromIntegral . floor)
                    >>= addOwnProperty "trunc" (VNative $ mathFunc $ fromIntegral . truncate)

                    >>= addOwnProperty "max" (VNative $ mathMaxFunc maximum)
                    >>= addOwnProperty "min" (VNative $ mathMaxFunc minimum)

                    >>= addOwnProperty "pow" (VNative $ mathFunc2 pow)
                    >>= addOwnProperty "atan2" (VNative $ mathFunc2 atan2)
                    >>= addOwnProperty "hypot" (VNative $ mathFunc2 hypot)

  this <- newObject >>= addOwnProperty "escape" (VNative objEscape)
                    >>= addOwnProperty "console" (VObj console)
                    >>= addOwnProperty "Function" (VObj function)
                    >>= addOwnProperty "String" (VObj string)
                    >>= addOwnProperty "Number" (VObj number)
                    >>= addOwnProperty "Boolean" (VObj boolean)
                    >>= addOwnProperty "Object" (VObj object)
                    >>= addOwnProperty "Array" (VObj array)
                    >>= addOwnProperty "Error" (VObj error)
                    >>= addOwnProperty "ReferenceError" (VObj error)
                    >>= addOwnProperty "Math" (VObj math)
                    >>= addOwnProperty "undefined" (VUndef)
                    >>= addOwnProperty "null" (VNull)
                    >>= addOwnProperty "eval" (VNative objEval)
                    >>= addOwnProperty "Infinity" (VNum $ 1 / 0)
                    >>= addOwnProperty "NaN" (VNum $ jsNaN)
                    >>= addOwnProperty "isNaN" (VNative objIsNaN)
  put $ JSGlobal (Just this)

makeObjectLiteral :: JSCxt -> [(PropertyName, Expr)] -> JSRuntime JSVal
makeObjectLiteral cxt nameValueList =
  let nameOf :: PropertyName -> String
      nameOf (IdentProp ident) = ident
      nameOf (StringProp str)  = str
      nameOf (NumProp n)       = show n
      addProp :: Shared JSObj -> (PropertyName, Expr) -> JSRuntime (Shared JSObj)
      addProp obj (propName, propValue) = do
        val <- runExprStmt cxt propValue >>= getValue
        addOwnProperty (nameOf propName) val obj
  in do
    obj <- newObject
    VObj <$> foldM addProp obj nameValueList

newObject :: JSRuntime (Shared JSObj)
newObject = do
  prototype <- share objectPrototype
  share JSObj { objClass = "Object",
                ownProperties = M.fromList [("prototype", VObj prototype)],
                callMethod = Nothing,
                primitive = Nothing }

objectPrototype :: JSObj
objectPrototype =
  JSObj { objClass = "Object",
          ownProperties =
            M.fromList [ ("prototype", VUndef),
                         ("toString", VNative objToString) ],
          callMethod = Nothing,
          primitive = Nothing }

objToString :: JSFunction
objToString this _args = toString this >>= return . VStr

toObject :: JSCxt -> JSVal -> JSRuntime JSVal
toObject _ v@(VObj obj) = return v
toObject cxt (VStr str) = runExprStmt cxt (FunCall (NewExpr (ReadVar "String") [Str str]) [])
toObject cxt (VException (_, v, _)) = return v
toObject cxt v = toString v >>= toObject cxt . VStr
