module Runtime (module Runtime, module X) where
import Control.Monad
import Control.Monad.State
import Control.Monad.Except
import Control.Monad.Writer
import Control.Applicative
import Data.Maybe
import Safe

import Runtime.Reference as X
import Runtime.Types as X
import Runtime.Object as X
import Runtime.Operations as X
import Runtime.Conversion as X
import Runtime.Global as X
import Runtime.Error as X
import Runtime.PropMap as X
import Parse
import Expr
import JSNum

import Debug.Trace

initialCxt :: Runtime JSCxt
initialCxt = JSCxt <$> initialEnv
                   <*> emptyEnv
                   <*> (VObj <$> getGlobalObject)
                   <*> pure NotStrict

initialEnv :: Runtime JSEnv
initialEnv = do
  global <- getGlobalObject
  share $ LexEnv (ObjEnvRec global) Nothing


emptyEnv :: Runtime JSEnv
emptyEnv = do
  m <- share emptyPropMap
  share $ LexEnv (DeclEnvRec m) Nothing

objToString :: JSFunction
objToString _this _args = return $ VStr "[object Object]"

objPrimitive :: JSFunction
objPrimitive (VObj this) _args = do
  objDefaultValue HintNone this
objPrimitive this _args = return this

toObject :: JSVal -> Runtime JSVal
toObject v@(VObj _) = return v
toObject (VStr str) = do
  obj <- newObject
  stringConstructor (VObj obj) [VStr str]
  -- runExprStmt cxt (FunCall (NewExpr (ReadVar "String") [Str str]) [])
toObject v = toString v >>= toObject . VStr


computeThisValue :: JSCxt -> JSVal -> JSVal
computeThisValue cxt v = case v of
  VRef ref ->
    if isPropertyReference ref
    then getBase ref
    else thisBinding cxt

  _ -> thisBinding cxt

-- ref 15.2.1.1
objFunction :: JSVal -> [JSVal] -> Runtime JSVal
objFunction _this args =
  let arg = headDef VUndef args
  in if arg == VUndef || arg == VNull
     then VObj <$> newObject
     else toObject (head args)

objConstructor :: JSVal -> [JSVal] -> Runtime JSVal
objConstructor _this _args = VObj <$> newObject

numConstructor :: JSVal -> [JSVal] -> Runtime JSVal
numConstructor this args = do
  num <- VNum <$> if null args
                  then return 0
                  else toNumber (head args)
  case this of
    VObj obj -> do
      VObj <$> (setClass "Number" obj >>= setPrimitiveValue num)
    _ -> raiseError $ "numConstructor called with this = " ++ show this

stringConstructor :: JSFunction
stringConstructor this args = do
  val <- VStr <$> if null args
                  then return ""
                  else toString (head args)
  case this of
    VObj obj -> do
      VObj <$> (setClass "String" obj >>= setPrimitiveValue val)
    _ -> raiseError $ "stringConstructor called with this = " ++ show this

arrayConstructor :: JSFunction
arrayConstructor this args =
  let len = VNum $ fromIntegral $ length args
  in case this of
    VObj obj -> do
      VObj <$> (setClass "Array" obj >>= addOwnProperty "length" len)
    _ -> raiseError $ "arrayConstructor called with this = " ++ show this

createArray :: [Maybe JSVal] -> Runtime JSVal
createArray vals =
  let len = VNum $ fromIntegral $ length vals
  in do
    obj <- newObject
    VObj <$> (setClass "Array" obj >>= addOwnProperty "length" len)


funConstructor :: JSVal -> [JSVal] -> Runtime JSVal
funConstructor _this [arg] = do
  body <- toString arg
  let Program strictness stmts = simpleParseInFunction body
  createFunction [] strictness stmts
funConstructor _this xs = error $ "Can't cstr Function with " ++ show xs

createFunction :: [Ident] -> Strictness -> [Statement] -> Runtime JSVal
createFunction paramList strict body = do
  newProto <- newObject
  objref <- newObject >>= setClass "Function"
                      >>= setCstrMethod (funcCall paramList strict body)
                      >>= setCallMethod (funcCall paramList strict body)
                      >>= addOwnProperty "prototype" (VObj newProto)
  return $ VObj objref

-- ref 13.2.1, incomplete
funcCall :: [Ident] -> Strictness -> [Statement] -> JSVal -> [JSVal] -> Runtime JSVal
funcCall paramList strict body this args =
  let makeRef env name = JSRef (VEnv env) name NotStrict
      newCxt cxt env = cxt { thisBinding = this, lexEnv = env, cxtStrictness = strict }
      addToNewEnv :: JSEnv -> Ident -> JSVal -> Runtime ()
      addToNewEnv env x v = putEnvironmentRecord (makeRef env x) v
  in do
    cxt <- getGlobalContext
    env <- newEnv (lexEnv cxt)
    zipWithM_ (addToNewEnv env) paramList args
    withNewContext (newCxt cxt env) $ do
      result <- jsRunStmts body
      case result of
        (CTReturn, Just v, _) -> return v
        (CTThrow, Just v, _)  -> throwError $ JSError (v, []) -- XXXX
        _ -> return VUndef


-- ref 15.1.2.1
objEval :: JSFunction
objEval _this args = case args of
  [] -> return VUndef
  (prog:_) -> do
    text <- toString prog
    result <- jsEvalCode text
    case result of
      (CTNormal, Just v, _) -> return v
      (CTThrow, Just v, _)  -> throwError $ JSError (v, []) -- XXXX
      _ -> return VUndef

stackTrace :: JSError -> String
stackTrace (JSError (err, stack)) = unlines $ show err : map show (reverse stack)
stackTrace (JSProtoError (t, msg)) = show (show t ++ ": " ++ msg)


printStackTrace :: JSError -> Runtime ()
printStackTrace = liftIO . putStrLn . stackTrace

propsFromList :: Ord k => [(k, a)] -> PropMap k (PropDesc a)
propsFromList = propMapFromList . map f
  where f (k, a) = (k, valueToProp a)

createGlobalObjectPrototype :: Runtime (Shared JSObj)
createGlobalObjectPrototype =
  share $ JSObj { objClass = "Object",
                  ownProperties =
                    propsFromList [ ("prototype", VUndef),
                                    ("toString", VNative objToString),
                                    ("prim", VNative objPrimitive) ],
                  objPrototype = Nothing,
                  callMethod = Nothing,
                  cstrMethod = Nothing }

createGlobalThis :: Runtime (Shared JSObj)
createGlobalThis = do
  prototype <- getGlobalObjectPrototype
  console <- newObject
  modifyRef console $ objSetProperty "log" (VNative jsConsoleLog)

  function <- newObject
  modifyRef function $ \obj -> obj { callMethod = Just funConstructor }

  object <- newObject >>= addOwnProperty "getOwnPropertyDescriptor" (VNative getOwnPropertyDescriptor)
                      >>= setCallMethod objFunction
                      >>= setCstrMethod objConstructor

  string <- newObject >>= isWrapperFor (\s -> VStr <$> toString s) (VStr "") "String"
  boolean <- newObject >>= isWrapperFor (return . VBool . toBoolean) (VBool False) "Boolean"
  number <- newObject >>= isWrapperFor (\s -> VNum <$> toNumber s) (VNum 0) "Number"
                      >>= addOwnProperty "NaN" (VNum jsNaN)
                      >>= addOwnProperty "isNaN" (VNative objIsNaN)


  array <- newObject >>= setCallMethod arrayConstructor

  -- error <- newObject
  -- modifyRef error $ \obj -> obj { callMethod = Just errConstructor }

  errorPrototype <- newObject >>= setClass "Error"
                              >>= addOwnProperty "toString" (VNative errorToString)
                              >>= addOwnProperty "prototype" (VObj prototype)

  errorObj <- newObject >>= setCallMethod errFunction
                        >>= setCstrMethod errConstructor
                        >>= addOwnProperty "prototype" (VObj errorPrototype)


  referenceError <- errorType "ReferenceError" (VObj errorPrototype)
  syntaxError    <- errorType "SyntaxError" (VObj errorPrototype)
  typeError      <- errorType "TypeError" (VObj errorPrototype)

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

  newObject >>= addOwnProperty "escape" (VNative objEscape)
            >>= addOwnProperty "console" (VObj console)
            >>= addOwnProperty "Function" (VObj function)
            >>= addOwnProperty "String" (VObj string)
            >>= addOwnProperty "Number" (VObj number)
            >>= addOwnProperty "Boolean" (VObj boolean)
            >>= addOwnProperty "Object" (VObj object)
            >>= addOwnProperty "Array" (VObj array)
            >>= addOwnProperty "Error" (VObj errorObj)
            >>= addOwnProperty "ReferenceError" (VObj referenceError)
            >>= addOwnProperty "SyntaxError" (VObj syntaxError)
            >>= addOwnProperty "TypeError" (VObj typeError)
            >>= addOwnProperty "Math" (VObj math)
            >>= addOwnProperty "undefined" (VUndef)
            >>= addOwnProperty "null" (VNull)
            >>= addOwnProperty "eval" (VNative objEval)
            >>= addOwnProperty "Infinity" (VNum $ 1 / 0)
            >>= addOwnProperty "NaN" (VNum $ jsNaN)
            >>= addOwnProperty "isNaN" (VNative objIsNaN)


errorType :: String -> JSVal -> Runtime (Shared JSObj)
errorType name parentPrototype = do
  prototype <-
    newObject >>= setClass "Error"
              >>= addOwnProperty "prototype" parentPrototype
              >>= addOwnProperty "name" (VStr name)

  newObject >>= setCallMethod errFunction
            >>= setCstrMethod errConstructor
            >>= addOwnProperty "prototype" (VObj prototype)

-- ref B.2.1, incomplete
objEscape :: JSFunction
objEscape _this args = case args of
  [] -> return VUndef
  (x:_) -> return x

jsConsoleLog :: JSVal -> [JSVal] -> Runtime JSVal
jsConsoleLog _this xs = tell (unwords (map showVal xs) ++ "\n") >> return VUndef

-- ref 15.2.3.3
getOwnPropertyDescriptor :: JSVal -> [JSVal] -> Runtime JSVal
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

putVar :: Ident -> JSVal -> Runtime ()
putVar x v = do
  cxt <- getGlobalContext
  putValue (ref cxt) v
    where ref cxt = JSRef (VEnv $ lexEnv cxt) x (cxtStrictness cxt)


-- ref 10.2.2.1
getIdentifierReference :: Maybe JSEnv -> Ident -> Strictness -> Runtime JSRef
getIdentifierReference Nothing name strict = return $ JSRef VUndef name strict
getIdentifierReference (Just lexRef) name strict = do
  env <- deref lexRef
  bound <- hasBinding name (envRec env)
  if bound
  then return $ JSRef (VEnv lexRef) name NotStrict
  else do
    getIdentifierReference (outer env) name strict


-- ref 11.13.1
assignRef :: JSVal -> JSVal -> Runtime JSVal
assignRef lref rref =
  case lref of
    VRef ref -> do
      rval <- getValue rref
      putValue ref rval
      return rval
    _ -> raiseReferenceError $ show lref ++ " is not assignable"



updateRef :: String -> JSVal -> JSVal -> Runtime JSVal
updateRef op lref rref =
  case lref of
    VRef ref -> do
      lval   <- getValue lref
      rval   <- getValue rref
      newVal <- evalBinOp op lval rval
      putValue ref newVal
      return newVal
    _ -> raiseReferenceError $ show lref ++ " is not assignable"

memberGet :: JSVal -> String -> Runtime JSVal
memberGet lval prop =
  case lval of
    VObj _ -> return $ VRef (JSRef lval prop NotStrict)
    _ -> raiseReferenceError $ "Cannot read property '" ++ prop ++ "' of " ++ show lval

funCall :: JSVal -> [JSVal] -> Runtime JSVal
funCall ref argList = do
  cxt <- getGlobalContext
  func <- getValue ref
  if typeof func == TypeUndefined
  then raiseReferenceError $ "Function " ++ getReferencedName (unwrapRef ref) ++ " is undefined"
  else let thisValue = computeThisValue cxt ref
       in objCall func thisValue argList





-- ref 13.2.2, incomplete
newObjectFromConstructor :: JSVal -> [JSVal] -> Runtime (Shared JSObj)
newObjectFromConstructor fun args = case fun of
  VRef (JSRef _ name _) -> create name =<< getValue fun
  _                     -> create (show fun) fun
  where
    create :: String -> JSVal -> Runtime (Shared JSObj)
    create name val = case val of
      VObj funref -> do
        obj <- newObject
        prototype <- objGetProperty "prototype" funref
        objSetPrototype (fromObj $ maybe VUndef propValue prototype) obj
        objCstr (VObj funref) (VObj obj) args
        return obj
      _ -> raiseError $ "Can't invoke constructor " ++ name
    fromObj (VObj obj) = Just obj
    fromObj _ = Nothing

objCall :: JSVal -> JSVal -> [JSVal] -> Runtime JSVal
objCall func this args = case func of
  VNative f -> f this args
  VObj objref -> deref objref >>= \obj -> case callMethod obj of
    Nothing -> raiseError "Can't call function: no callMethod"
    Just method -> method this args
  VUndef -> raiseError "Undefined function"
  _ -> raiseError $ "Can't call " ++ show func

objCstr :: JSVal -> JSVal -> [JSVal] -> Runtime JSVal
objCstr func this args = case func of
  VNative f -> f this args
  VObj objref -> deref objref >>= \obj -> case cstrMethod obj of
    Nothing -> raiseError "Can't call function: no cstrMethod"
    Just method -> method this args
  VUndef -> raiseError "Undefined function"
  _ -> raiseError $ "Can't call " ++ show func
