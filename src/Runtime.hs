{-# Language LambdaCase #-}

module Runtime (module Runtime, module X) where
import Control.Monad
import Control.Monad.State
import Control.Monad.Except
import Control.Monad.Writer
import Control.Applicative
import Control.Arrow
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
import Runtime.PropDesc as X
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

toObject :: JSVal -> Runtime (Shared JSObj)
toObject (VObj objRef) = return objRef
toObject (VStr str) = do
  obj <- newObject
  stringConstructor (VObj obj) [VStr str]
  return obj
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
     else VObj <$> toObject (head args)

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


arrayAssigns :: [Maybe a] -> [(Integer, a)]
arrayAssigns xs = map (second fromJust) $ filter (isJust.snd) $ zip [0..] xs

createArray :: [Maybe JSVal] -> Runtime JSVal
createArray vals =
  let len = VNum $ fromIntegral $ length vals
      assigns = arrayAssigns vals
  in do obj <- newObject >>= setClass "Array"
                         >>= addOwnProperty "length" len
                         >>= setArrayIndices assigns
        return $ VObj obj

setArrayIndices :: [(Integer, JSVal)] -> Shared JSObj -> Runtime (Shared JSObj)
setArrayIndices assigns objRef = do
  mapM_ (\(n, v) -> objPut (show n) v False objRef) assigns
  return objRef


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
    arguments <- newObject >>= addOwnProperty "callee" (VNum 42)
    addToNewEnv env "arguments" (VObj arguments)
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
                                    ("hasOwnProperty", VNative objHasOwnProperty),
                                    ("valueOf", VNative objValueOf) ],
                  objPrototype = Nothing,
                  callMethod = Nothing,
                  cstrMethod = Nothing,
                  objExtensible = True }

createGlobalThis :: Runtime (Shared JSObj)
createGlobalThis = do
  prototype <- getGlobalObjectPrototype
  console <- newObject
  modifyRef console $ objSetProperty "log" (VNative jsConsoleLog)

  functionPrototype <- newObject
  function <- newObject >>= setCallMethod funConstructor
                        >>= objSetPrototype functionPrototype
                        >>= addOwnConstant "length" (VNum 1) -- ref 15.3.3.2

  object <- newObject >>= addOwnProperty "getOwnPropertyDescriptor" (VNative getOwnPropertyDescriptor)
                      >>= addOwnProperty "prototype" (VObj prototype)
                      >>= addOwnProperty "defineProperty" (VNative objDefineProperty)
                      >>= addOwnProperty "preventExtensions" (VNative objPreventExtensions)
                      >>= setCallMethod objFunction
                      >>= setCstrMethod objConstructor

  string <- newObject >>= isWrapperFor (\s -> VStr <$> toString s) (VStr "") "String"
  boolean <- newObject >>= isWrapperFor (return . VBool . toBoolean) (VBool False) "Boolean"
  number <- newObject >>= isWrapperFor (\s -> VNum <$> toNumber s) (VNum 0) "Number"
                      >>= addOwnProperty "isNaN" (VNative objIsNaN)
                      >>= addReadOnlyConstants numberConstants


  array <- newObject >>= setCallMethod arrayConstructor
                     >>= setCstrMethod arrayConstructor
                     >>= addOwnProperty "prototype" (VObj prototype)

  errorPrototype <- newObject >>= setClass "Error"
                              >>= addOwnProperty "toString" (VNative errorToString)
                              >>= addOwnProperty "prototype" (VObj prototype)

  errorObj <- newObject >>= setCallMethod errFunction
                        >>= setCstrMethod errConstructor
                        >>= addOwnProperty "prototype" (VObj errorPrototype)


  referenceError <- errorType "ReferenceError" (VObj errorPrototype)
  syntaxError    <- errorType "SyntaxError" (VObj errorPrototype)
  typeError      <- errorType "TypeError" (VObj errorPrototype)


  datePrototype <- newObject >>= setClass "Date"
                             >>= objSetPrototype prototype
                             >>= setCstrMethod dateConstructor
                             >>= addOwnProperty "toString" (VNative dateToString)
                             >>= addOwnProperty "valueOf" (VNative dateValueOf)

  date <- newObject >>= setClass "Date"
                    >>= setCallMethod dateFunction
                    >>= setCstrMethod dateConstructor
                    >>= objSetPrototype functionPrototype
                    >>= addOwnProperty "prototype" (VObj datePrototype)

  math <- mathObject
  json <- newObject >>= addOwnProperty "stringify" (VNative jsonStringify)

  newObject >>= addOwnProperty "escape" (VNative objEscape)
            >>= addOwnProperty "console" (VObj console)
            >>= addOwnProperty "Function" (VObj function)
            >>= addOwnProperty "String" (VObj string)
            >>= addOwnProperty "Number" (VObj number)
            >>= addOwnProperty "Boolean" (VObj boolean)
            >>= addOwnProperty "Object" (VObj object)
            >>= addOwnProperty "Array" (VObj array)
            >>= addOwnProperty "Error" (VObj errorObj)
            >>= addOwnProperty "Date" (VObj date)
            >>= addOwnProperty "ReferenceError" (VObj referenceError)
            >>= addOwnProperty "SyntaxError" (VObj syntaxError)
            >>= addOwnProperty "TypeError" (VObj typeError)
            >>= addOwnProperty "Math" (VObj math)
            >>= addOwnProperty "JSON" (VObj json)
            >>= addOwnProperty "eval" (VNative objEval)
            >>= addOwnProperty "isNaN" (VNative objIsNaN)
            >>= addOwnConstant "Infinity" (VNum $ 1 / 0)
            >>= addOwnConstant "NaN" (VNum $ jsNaN)
            >>= addOwnConstant "undefined" (VUndef)
            >>= addOwnConstant "null" (VNull)


mathObject :: Runtime (Shared JSObj)
mathObject = do
  newObject >>= addReadOnlyConstants mathConstants
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

mathConstants :: [(String, JSNum)]
mathConstants = allToJSNum [ ("PI", pi),
                             ("E", exp 1),
                             ("LN2", log 2),
                             ("LN10", log 10),
                             ("LOG10E", 1 / log 10),
                             ("LOG2E", 1 / log 2) ]
  where allToJSNum = map (second JSNum)


numberConstants :: [(String, JSNum)]
numberConstants = [ ("NaN", JSNum $ 0/0),
                    ("POSITIVE_INFINITY", JSNum $ 1/0),
                    ("NEGATIVE_INFINITY", JSNum $ -1/0),
                    ("MAX_VALUE", jsMaxValue),
                    ("MIN_VALUE", jsMinValue) ]

addReadOnlyConstants :: [(String, JSNum)] -> Shared JSObj -> Runtime (Shared JSObj)
addReadOnlyConstants xs obj = do
  forM xs $ \(name, value) -> addOwnConstant name (VNum value) obj
  return obj

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
  then return $ JSRef (VEnv lexRef) name strict
  else do
    getIdentifierReference (outer env) name strict


-- ref 11.13.1
assignRef :: JSVal -> JSVal -> Runtime JSVal
assignRef lref rref =
  case lref of
    VRef ref -> do
      rval <- getValue rref
      disallowEvalAssignment ref
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
      disallowEvalAssignment ref
      putValue ref newVal
      return newVal
    _ -> raiseReferenceError $ show lref ++ " is not assignable"

-- ref 11.13.1
disallowEvalAssignment :: JSRef -> Runtime ()
disallowEvalAssignment (JSRef (VEnv _) name strict)
  | name /= "eval" && name /= "arguments" = debug name >> return ()
  | strict == NotStrict                   = debug "b" >> return ()
  | otherwise = cannotAssignTo name
disallowEvalAssignment x = debug x >> return ()

cannotAssignTo :: String -> Runtime ()
cannotAssignTo name = raiseSyntaxError $ "Assignment of " ++ name ++ " in strict mode"

memberGet :: JSVal -> String -> Runtime JSVal
memberGet lval prop = do
  strict <- getGlobalStrictness
  case lval of
    VObj _ -> return $ VRef (JSRef lval prop strict)
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
        prop <- objGetProperty "prototype" funref
        prototype <- fromObj prop
        setPrototype prototype obj
        objCstr (VObj funref) (VObj obj) args
        return obj
      _ -> raiseError $ "Can't invoke constructor " ++ name
    fromObj :: Maybe (PropDesc JSVal) -> Runtime (Maybe (Shared JSObj))
    fromObj Nothing = return Nothing
    fromObj (Just desc) = do
      val <- propValue desc
      case val of
        VObj obj -> return $ Just obj
        _        -> return Nothing
    setPrototype :: Maybe (Shared JSObj) -> Shared JSObj -> Runtime (Shared JSObj)
    setPrototype mp = updateObj $ \obj -> obj { objPrototype = mp }

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
    Nothing -> raiseError "Can't create object: function has no cstrMethod"
    Just method -> method this args
  VUndef -> raiseError "Undefined function"
  _ -> raiseError $ "Can't call " ++ show func

first1 :: [JSVal] -> JSVal
first1 xs = a where [a] = take 1 (xs ++ repeat VUndef)

first2 :: [JSVal] -> (JSVal, JSVal)
first2 xs = (a,b) where [a,b] = take 2 (xs ++ repeat VUndef)

first3 :: [JSVal] -> (JSVal, JSVal, JSVal)
first3 xs = (a,b,c) where [a,b,c] = take 3 (xs ++ repeat VUndef)

-- ref 15.2.3.3
getOwnPropertyDescriptor :: JSVal -> [JSVal] -> Runtime JSVal
getOwnPropertyDescriptor _this args =
  let (o, p) = first2 args
  in case o of
    VObj obj -> do
      name <- toString p
      desc <- objGetOwnProperty name obj
      fromPropertyDescriptor desc
    _ -> raiseTypeError "Object.getOwnPropertyDescriptor called on non-object"

-- ref 15.2.3.6
objDefineProperty :: JSVal -> [JSVal] -> Runtime JSVal
objDefineProperty _this args =
  let (o, p, attrs) = first3 args
  in case o of
    VObj obj -> do
      name <- toString p
      desc <- toPropertyDescriptor attrs
      objDefineOwnProperty name desc True obj
      return o
    _ -> raiseTypeError "Object.defineProperty called on non-object"

-- ref 15.2.3.10
objPreventExtensions :: JSVal -> [JSVal] -> Runtime JSVal
objPreventExtensions _this args =
  let o = first1 args
  in case o of
    VObj obj -> do
      objSetExtensible False obj
      return o
    _ -> raiseTypeError "Object.preventExtensions called on non-object"



-- ref 15.2.4.4, incomplete
objValueOf :: JSVal -> [JSVal] -> Runtime JSVal
objValueOf this _args = VObj <$> toObject this

-- ref 15.2.4.5
objHasOwnProperty :: JSVal -> [JSVal] -> Runtime JSVal
objHasOwnProperty this args =
  let v = first1 args
  in do
    p <- toString v
    o <- toObject this
    VBool . isJust <$> objGetOwnProperty p o


jsonStringify :: JSVal -> [JSVal] -> Runtime JSVal
jsonStringify _this _args = return $ VStr "not implemented"

functionIsConstructor :: JSFunction -> JSFunction
functionIsConstructor cstr _this args = do
  this <- newObject
  cstr (VObj this) args

dateFunction :: JSVal -> [JSVal] -> Runtime JSVal
dateFunction = functionIsConstructor dateConstructor

dateConstructor :: JSVal -> [JSVal] -> Runtime JSVal
dateConstructor this _ = case this of
  VObj objRef -> VObj <$> (setClass "Date" objRef)

dateToString :: JSVal -> [JSVal] -> Runtime JSVal
dateToString _ _ = return $ VStr "1969-12-30 21:18:57 UTC"

dateValueOf :: JSVal -> [JSVal] -> Runtime JSVal
dateValueOf _ _ = return $ VNum 142857

-- ref 8.10.5, incomplete
toPropertyDescriptor :: JSVal -> Runtime (PropDesc JSVal)
toPropertyDescriptor (VObj objRef) = do
  enum <- toBoolean <$> objGet "enumerable" objRef
  conf <- toBoolean <$> objGet "configurable" objRef
  value <- objGet "value" objRef
  writable <- toBoolean <$> objGet "writable" objRef
  get <- objGet "get" objRef >>= mkGetter
  set <- objGet "set" objRef >>= mkSetter

  if isJust get || isJust set
  then return $ AccessorPD get set enum conf
  else return $ DataPD value writable enum conf

toPropertyDescriptor other = raiseProtoError TypeError $ "Can't convert " ++ show other ++ " to type descritor"

mkGetter :: JSVal -> Runtime (Maybe (Runtime JSVal))
mkGetter (VObj obj) = do
  call <- callMethod <$> deref obj
  case call of
    Nothing -> return Nothing
    Just f -> return $ Just (f VUndef [])
mkGetter _ = return Nothing


mkSetter :: JSVal -> Runtime (Maybe (JSVal -> Runtime ()))
mkSetter (VObj obj) = do
  call <- callMethod <$> deref obj
  return $ Just (\a -> raiseSyntaxError "mkSetter undefined")
mkSetter _ = return Nothing
