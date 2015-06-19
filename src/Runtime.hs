{-# Language LambdaCase #-}

module Runtime (module Runtime, module X) where
import Control.Monad
import Control.Monad.State
import Control.Monad.Except
import Control.Monad.Writer
import Control.Monad.Fix
import Control.Applicative
import Control.Arrow
import Text.Printf
import Data.Maybe
import Safe

import Runtime.Reference as X
import Runtime.Types as X
import Runtime.Object as X
import Runtime.Operations as X
import Runtime.Conversion as X
import Runtime.Global as X
import Runtime.Error as X
import Runtime.Arguments as X
import Runtime.PropMap as X
import Runtime.PropDesc as X
import Runtime.Function as X
import Runtime.Prototype as X
import Runtime.PropertyDescriptor as X
import JSNum as X
import Parse
import Expr
import Core

import Debug.Trace

initialCxt :: Runtime JSCxt
initialCxt = do
  env <- initialEnv
  obj <- VObj <$> getGlobalObject
  return $ JSCxt env env obj NotStrict

initialEnv :: Runtime JSEnv
initialEnv = do
  global <- getGlobalObject
  share $ LexEnv (ObjEnvRec global False) Nothing

createGlobalObjectPrototype :: Runtime (Shared JSObj)
createGlobalObjectPrototype =
  newObject >>= addOwnProperty "prototype" VUndef
            >>= addMethod "toString" 0 objToString
            >>= addMethod "hasOwnProperty" 1 objHasOwnProperty
            >>= addMethod "valueOf" 0 objValueOf
            >>= addMethod "isPrototypeOf" 1 objIsPrototypeOf
            >>= addMethod "propertyIsEnumerable" 1 objPropertyIsEnumerable

createGlobalThis :: Runtime (Shared JSObj)
createGlobalThis = do
  prototype <- getGlobalObjectPrototype

  functionPrototype <- newObject
    >>= setCallMethod (\_this _args -> return VUndef)
    >>= addOwnProperty "length" (VNum 0)
    >>= addMethod "call" 1 funCallMethod
    >>= addMethod "bind" 1 funBind
    >>= addMethod "apply" 2 funApply

  function <- newObject
    >>= setClass "Function"
    >>= addOwnProperty "prototype" (VObj functionPrototype)
    >>= addOwnProperty "name" (VStr "Function")
    >>= objSetHasInstance funHasInstance
    >>= setCallMethod (funFunction functionPrototype)
    >>= setCstrMethod funConstructor
    >>= objSetPrototype functionPrototype
    >>= addOwnProperty "prototype" (VObj functionPrototype)
    >>= addOwnProperty "length" (VNum 1) -- ref 15.3.3.2

  object <- functionObject "Object" prototype
    >>= addMethod "getOwnPropertyDescriptor" 2 getOwnPropertyDescriptor
    >>= addMethod "getOwnPropertyNames"      1 getOwnPropertyNames
    >>= addMethod "defineProperty"           3 objDefineProperty
    >>= addMethod "preventExtensions"        1 objPreventExtensions
    >>= addMethod "getPrototypeOf"           1 objGetPrototypeOf
    >>= setCallMethod objFunction
    >>= setCstrMethod objConstructor
  addOwnProperty "prototype" (VObj prototype) object
  addOwnProperty "constructor" (VObj object) prototype

  newObject
    >>= addOwnProperty "Object" (VObj object)
    >>= addOwnProperty "Function" (VObj function)

objToString :: JSFunction
objToString this _args = do
  cls <- objClass <$> deref (toObj this)
  return $ VStr ("[object " ++ cls ++ "]")

objPrimitive :: JSFunction
objPrimitive (VObj this) _args = do
  objDefaultValue HintNone this
objPrimitive this _args = return this


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


arrayAssigns :: [Maybe a] -> [(Integer, a)]
arrayAssigns xs = map (second fromJust) $ filter (isJust.snd) $ zip [0..] xs

createArray :: [Maybe JSVal] -> Runtime JSVal
createArray vals =
  let len = VNum $ fromIntegral $ length vals
      assigns = arrayAssigns vals
  in do cstr <- getGlobalProperty "Array"
        arrayPrototype <- objGet "prototype" (toObj cstr)
        obj <- newObject >>= setClass "Array"
                         >>= objSetPrototype (toObj arrayPrototype)
                         >>= addOwnPropertyDescriptor "constructor" (dataPD cstr True False True)
                         >>= addOwnPropertyDescriptor "length" (dataPD len True False False)
                         >>= setArrayIndices assigns
        return $ VObj obj

setArrayIndices :: [(Integer, JSVal)] -> Shared JSObj -> Runtime (Shared JSObj)
setArrayIndices assigns objRef = do
  mapM_ (\(n, v) -> defineOwnProperty (show n) (dataPD v True True True) False objRef) assigns
  return objRef


funFunction :: Shared JSObj -> JSFunction
funFunction prototype _this args = do
  obj <- newObject >>= setClass "Function"
                   >>= objSetPrototype prototype
  funConstructor (VObj obj) args

-- ref 15.3.2.1
funConstructor :: JSVal -> [JSVal] -> Runtime JSVal
funConstructor this args = case this of
  VObj objRef -> do
    let arg = lastDef VUndef args
        paramList = initDef [] args

    body <- toString arg
    params <- mapM toString paramList

    case parseInFunction body of
      Left err -> raiseSyntaxError (show err)
      Right (Program strictness stmts) -> do
        globalEnv <- getGlobalEnvironment
        createFunction Nothing params strictness stmts globalEnv
  _ -> raiseTypeError "Function constructor"

funCallMethod :: JSFunction
funCallMethod this args = do
  isCallable this >>= \case
    Nothing   -> raiseTypeError $ "Not a function: " ++ show this
    Just call -> call (first1 args) (tail1 args)

-- ref 15.3.4.3
funApply :: JSFunction
funApply this args =
  let (thisArg, argArray) = first2 args
  in do
    call <- findCallMethod this
    case argArray of
      VNull     -> call thisArg []
      VUndef    -> call thisArg []
      VObj obj  -> funApply' call thisArg obj
      otherwise -> raiseTypeError "apply: Arguments list has wrong type"

  where
    funApply' call thisArg argArray = do
      n <- objGet "length" argArray >>= toInt32
      args <- forM [0..n] $ \index -> objGet (show index) argArray
      call thisArg args

    findCallMethod :: JSVal -> Runtime JSFunction
    findCallMethod this = do
      originalCallMethod <- isCallable this
      case originalCallMethod of
        Nothing -> raiseTypeError "Cannot apply non-function"
        Just call -> return call


-- ref 15.3.4.5
funBind :: JSFunction
funBind this args = do
  originalCallMethod <- isCallable this
  case originalCallMethod of
    Nothing -> raiseTypeError "Cannot bind non-function"
    Just targetCall -> do
      let (VObj target) = this

      funPrototype <- objFindPrototype "Function"

      targetCstr   <- cstrMethod <$> deref target
      targetClass  <- objClassName target
      targetLength <- objGet "length" target >>= toInt

      let (thisArg, a) = if null args then (VUndef, []) else (head args, tail args)
          l = if targetClass == "Function"
              then max 0 $ targetLength - length a
              else 0
          cstrMethod = case targetCstr of
            Nothing -> funCstrError
            Just m  -> funCstrBound m          thisArg a
          callMethod = funCallBound targetCall thisArg a

      f <- newObject >>= setGetMethod funGet
                     >>= setClass "Function"
                     >>= objSetPrototype funPrototype
                     >>= setCallMethod callMethod
                     >>= setCstrMethod cstrMethod
                     >>= objSetHasInstance funHasInstanceBound
                     >>= addOwnPropertyDescriptor "length" (dataPD (VNum $ fromIntegral l) False False False)
                     >>= objSetExtensible True
                     >>= addOwnPropertyDescriptor "caller" throwerProperty
                     >>= addOwnPropertyDescriptor "arguments" throwerProperty
      return (VObj f)

-- ref 15.3.4.5.1
funCallBound :: JSFunction -> JSVal -> [JSVal] -> JSFunction
funCallBound targetCall boundThis boundArgs _this args = do
  targetCall boundThis (boundArgs ++ args)


-- ref 15.3.4.5.2
funCstrBound :: JSFunction -> JSVal -> [JSVal] -> JSFunction
funCstrBound targetCstr boundThis boundArgs _this args = do
  targetCstr boundThis (boundArgs ++ args)

funCstrError :: JSFunction
funCstrError _ _ = raiseTypeError "Cannot construct object"

-- ref 15.3.4.5.3
funHasInstanceBound :: Shared JSObj -> JSVal -> Runtime a
funHasInstanceBound = undefined



-- ref 13.2
createFunction :: Maybe Ident -> [Ident] -> Strictness -> [Statement] -> Shared LexEnv -> Runtime JSVal
createFunction name paramList strict body scope =
  VObj <$> buildFunction
    where
      buildFunction :: Runtime (Shared JSObj)
      buildFunction = do
        functionPrototype <- objFindPrototype "Function"
        prototype <- newObject

        func <- newObject
          >>= setClass "Function" -- (3)
          >>= objSetPrototype functionPrototype -- (4)
          >>= setGetMethod funGet -- (5)
          >>= objSetHasInstance (funHasInstance) -- (8)
          >>= setScope scope
          >>= setFormalParameters paramList
          >>= setCode (Program strict body)
          >>= objSetExtensible True
          >>= addOwnPropDesc "length" lengthProperty
          >>= addOwnPropDesc "prototype" (prototypeProperty $ VObj prototype)

        defineOwnProperty "constructor" (dataPD (VObj func) True False True) False prototype
        setCallMethod (callMethod $ VObj func) func -- (6) XXX
        setCstrMethod (callMethod $ VObj func) func -- (7) XXX

        ifStrictContext $ do
          let prop = throwerProperty
          defineOwnProperty "caller" prop False func
          defineOwnProperty "arguments" prop False func

        return func

      lengthProperty = dataPD nparams False False True
      nparams = VNum $ fromIntegral $ length paramList
      prototypeProperty prototype = dataPD prototype True False False
      nameProperty = fromMaybe "" name
      callMethod func = funcCall name func paramList strict body

throwerProperty :: PropDesc JSVal
throwerProperty = accessorPD (Just thrower) (Just (const thrower)) False False

thrower :: a -> Runtime b
thrower _ = raiseTypeError "Cannot access property"

-- ref 15.3.5.4
funGet :: String -> Shared JSObj -> Runtime JSVal
funGet p f = do
  val <- objGetObj p f
  if p /= "caller"
  then return val
  else case val of
    VObj obj -> do
      prog <- objCode <$> deref obj
      case prog of
        Just (Program Strict _) ->
          raiseTypeError "Cannot access caller property"
        _ -> return val

-- ref 13.2.1
-- ref 10.4.3
funcCall :: Maybe Ident -> JSVal -> [Ident] -> Strictness -> [Statement] -> JSVal -> [JSVal] -> Runtime JSVal
funcCall name func paramList strict body this args =
  let makeRef env name = JSRef (VEnv env) name NotStrict
      newCxt cxt env newThis = cxt { thisBinding = newThis, lexEnv = env, varEnv = env, cxtStrictness = strict }
      addToNewEnv :: EnvRec -> Ident -> JSVal -> Runtime ()
      addToNewEnv env x v = putValue (makeRef env x) v
      findNewThis :: Strictness -> JSVal -> Runtime JSVal
      findNewThis Strict    this   = return this
      findNewThis NotStrict VNull  = VObj <$> getGlobalObject
      findNewThis NotStrict VUndef = VObj <$> getGlobalObject
      findNewThis NotStrict other  = VObj <$> toObject other
  in do
    cxt <- getGlobalContext

    scope <- objScope <$> deref (toObj func)
    localEnv <- newDeclarativeEnvironment scope
    env <- envRec <$> deref localEnv
    zipWithM_ (addToNewEnv env) paramList (args ++ repeat VUndef)
    case name of
      Just n -> addToNewEnv env n func
      _      -> return ()
    newThis <- findNewThis strict this
    withNewContext (newCxt cxt localEnv newThis) $ do
      performDBI DBIFunction strict body
      when ("arguments" `notElem` paramList) $ do
        VObj argsObj <- createArgumentsObject func paramList args env strict
        addToNewEnv env "arguments" (VObj argsObj)
      result <- jsRunStmts body
      case result of
        CTReturn (Just v) -> return v
        CTThrow  (Just v) -> rethrowWithStack v
        otherwise         -> return VUndef

rethrowWithStack :: JSVal -> Runtime a
rethrowWithStack v = do
  stack <- case v of
    VObj obj -> objGet "stack" obj >>= \case
                VStacktrace st -> return st
                _              -> return []
    _ -> return []
  throwError $ JSError (v, stack)

getStackTrace :: JSVal -> Runtime [SrcLoc]
getStackTrace (VObj obj) = do
  st <- objGet "stack" obj
  return $ case st of
    VStacktrace s -> s
    _ -> []
getStackTrace _ = return []

-- ref 15.1.2.1
objEval :: EvalCallType -> JSFunction
objEval callType _this args = case args of
  [] -> return VUndef
  (prog:_) -> do
    text <- toString prog
    result <- jsEvalCode callType text
    case result of
      CTNormal (Just v) -> return v
      CTThrow  (Just v) -> do
        stackTrace <- getStackTrace v
        throwError $ JSError (v, stackTrace)
      _ -> return VUndef

-- ref 15.1.2.2
parseInt :: JSFunction
parseInt _this args =
  let arg = first1 args
  in do
    str <- toString arg
    return $ VNum $ parseNumber str

-- ref 15.1.2.3
parseFloat :: JSFunction
parseFloat _this args =
  let arg = first1 args
  in do
    str <- toString arg
    return $ VNum $ parseNumber str

-- ref 15.1.2.4
objIsNaN :: JSFunction
objIsNaN _this args =
  let arg = first1 args
  in VBool . isNaN . fromJSNum <$> toNumber arg

objIsFinite :: JSFunction
objIsFinite this args =
  let arg = first1 args
  in VBool . isFinite <$> toNumber arg
    where isFinite (JSNum x)
            | isNaN x   = False
            | x == 1/0  = False
            | x == -1/0 = False
            | otherwise = True

stackTrace :: JSError -> String
stackTrace (JSError (err, stack)) = unlines $ show err : map show (reverse stack)
stackTrace (JSProtoError (t, msg)) = show (show t ++ ": " ++ msg)


printStackTrace :: JSError -> Runtime ()
printStackTrace = liftIO . putStrLn . stackTrace

propsFromList :: Ord k => [(k, a)] -> PropMap k (PropDesc a)
propsFromList = propMapFromList . map f
  where f (k, a) = (k, dataPD a True True True)

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
  ref <- getIdentifierReference (Just $ lexEnv cxt) x (cxtStrictness cxt)
  void $ assignRef (VRef ref) v


-- ref 10.2.2.1
getIdentifierReference :: Maybe JSEnv -> Ident -> Strictness -> Runtime JSRef
getIdentifierReference Nothing name strict = return $ JSRef VUndef name strict
getIdentifierReference (Just lexRef) name strict = do
  env <- deref lexRef
  exists <- hasBinding name (envRec env)
  if exists
  then return (JSRef (VEnv $ envRec env) name strict)
  else getIdentifierReference (outer env) name strict

-- ref 10.2.2.2
newDeclarativeEnvironment :: Maybe JSEnv -> Runtime JSEnv
newDeclarativeEnvironment e = do
  envRec <- share emptyPropMap
  share $ LexEnv (DeclEnvRec envRec) e


-- ref 10.2.2.3
newObjectEnvironment :: Shared JSObj -> Maybe JSEnv -> Bool -> Runtime JSEnv
newObjectEnvironment obj oldEnv provideThis = share $ LexEnv (ObjEnvRec obj provideThis) oldEnv


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
  | name /= "eval" && name /= "arguments" = return ()
  | strict == NotStrict                   = return ()
  | otherwise = cannotAssignTo name
disallowEvalAssignment x = return ()

cannotAssignTo :: String -> Runtime ()
cannotAssignTo name = raiseSyntaxError $ "Assignment of " ++ name ++ " in strict mode"

-- ref. 11.2.1
memberGet :: JSVal -> String -> Runtime JSVal
memberGet lval prop = do
  strict <- getGlobalStrictness
  return $ VRef (JSRef lval prop strict)

-- ref 11.2.3
callFunction :: JSVal -> [JSVal] -> Runtime JSVal
callFunction ref argList = do
  cxt <- getGlobalContext
  func <- getValue ref
  thisValue <- computeThisValue cxt ref
  case evalCallType ref func of
    DirectEvalCall -> objEval DirectEvalCall thisValue argList
    _              -> do
      assertFunction name callMethod func
      objCall func thisValue argList

  where
    computeThisValue cxt v = case v of
      VRef ref ->
        if isPropertyReference ref
        then return (getBase ref)
        else case getBase ref of
          VEnv env -> return (implicitThisValue env)

      _ -> return VUndef

    implicitThisValue (DeclEnvRec _) = VUndef
    implicitThisValue (ObjEnvRec obj True) = (VObj obj)
    implicitThisValue (ObjEnvRec _ False) = VUndef

    name = case ref of
      VRef (JSRef _ name _) -> name
      _ -> ""

assertFunction :: String -> (JSObj -> Maybe a) -> JSVal -> Runtime ()
assertFunction name m val =
  case val of
    VNative _ _ _ -> return ()
    VObj o -> do
      m <$> deref o >>= \case
        Just _ -> return ()
        Nothing -> error "is not a function"
    VUndef -> error "is undefined"
    _ -> error "is not a function"

  where error reason = raiseTypeError $ unwords [name, reason]

-- ref 15.1.2.1.1
evalCallType :: JSVal -> JSVal -> EvalCallType
evalCallType (VRef (JSRef (VEnv env) "eval" _)) (VNative "eval" _ _) = DirectEvalCall
evalCallType _ _                                                     = IndirectEvalCall


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
        prototype <- objGetProperty "prototype" funref >>= fromObj
        case prototype of
          Just p -> setPrototype prototype obj
          Nothing -> do
            objPrototype <- Just <$> getGlobalObjectPrototype
            setPrototype objPrototype obj
        defineOwnProperty "constructor" (dataPD val True False True) False obj
        objCstr (VObj funref) (VObj obj) args >>= \case
          VObj o -> return o
          _ -> return obj
      _ -> raiseError $ "Can't invoke constructor " ++ name
    fromObj :: Maybe (PropDesc JSVal) -> Runtime (Maybe (Shared JSObj))
    fromObj Nothing = return Nothing
    fromObj (Just desc) = do
      val <- return $ fromMaybe VUndef $ propValue desc
      case val of
        VObj obj -> return $ Just obj
        _        -> return Nothing
    setPrototype :: Maybe (Shared JSObj) -> Shared JSObj -> Runtime (Shared JSObj)
    setPrototype mp = updateObj $ \obj -> obj { objPrototype = mp }

objCall :: JSVal -> JSVal -> [JSVal] -> Runtime JSVal
objCall func this args = case func of
  VNative _ _ f -> f this args
  VObj objref -> deref objref >>= \obj -> case callMethod obj of
    Nothing -> raiseError "Can't call function: no callMethod"
    Just method -> method this args
  VUndef -> raiseReferenceError "Function is undefined"
  _ -> raiseError $ "Can't call " ++ show func

objCstr :: JSVal -> JSVal -> [JSVal] -> Runtime JSVal
objCstr func this args = case func of
  VNative _ _ f -> f this args
  VObj objref -> deref objref >>= \obj -> case cstrMethod obj of
    Nothing -> raiseError "Can't create object: function has no cstrMethod"
    Just method -> method this args
  VUndef -> raiseError "Undefined function"
  _ -> raiseError $ "Can't call " ++ show func


-- ref 15.2.3.2
objGetPrototypeOf :: JSFunction
objGetPrototypeOf _this args =
  let o = first1 args
  in case o of
    VObj obj -> maybe VUndef VObj . objPrototype <$> deref obj
    _        -> raiseTypeError "Object.getPrototypeOf called on non-object"

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

-- ref 15.2.3.4
getOwnPropertyNames :: JSFunction
getOwnPropertyNames _this args =
  let o = first1 args
  in case o of
    VObj obj -> do
      ks <- propMapKeys . ownProperties <$> deref obj
      createArray $ map (Just . VStr) ks





-- ref 15.2.3.6
objDefineProperty :: JSVal -> [JSVal] -> Runtime JSVal
objDefineProperty _this args =
  let (o, p, attrs) = first3 args
  in case o of
    VObj obj -> do
      name <- toString p
      desc <- toPropertyDescriptor attrs
      defineOwnProperty name desc True obj
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

-- ref 15.2.4.6, incomplete
objIsPrototypeOf :: JSFunction
objIsPrototypeOf this args =
  let arg = first1 args
      findPrototype o Nothing = return False
      findPrototype o (Just v) =
        if o == v
        then return True
        else do
          v' <- objPrototype <$> deref v
          findPrototype o v'
  in do
    case arg of
      VObj v -> do
        o <- toObject this
        v' <- objPrototype <$> deref v
        result <- findPrototype o v'
        return $ VBool result

-- ref 15.2.4.7
objPropertyIsEnumerable :: JSFunction
objPropertyIsEnumerable this args = let v = first1 args in do
  p <- toString v
  o <- toObject this
  desc <- objGetOwnProperty p o
  return . VBool . maybe False propIsEnumerable $ desc

objId :: JSVal -> [JSVal] -> Runtime JSVal
objId (VObj (Shared _ oid)) _args = return $ VNum $ fromIntegral oid
objId x _ = raiseTypeError $ "No objId for " ++ show x

-- ref 15.2.4.5
objHasOwnProperty :: JSVal -> [JSVal] -> Runtime JSVal
objHasOwnProperty this args =
  let v = first1 args
  in do
    p <- toString v
    o <- toObject this
    VBool . isJust <$> objGetOwnProperty p o

-- ref 10.5
performDBI :: DBIType -> Strictness -> [Statement] -> Runtime ()
performDBI dbiType strict stmts = bindAll dbiType strict (declBindings stmts)
  where
    vars      = concatMap searchVariables stmts
    functions = concatMap searchFunctions stmts

bindAll :: DBIType -> Strictness -> [(Ident, Expr)] -> Runtime ()
bindAll dbiType strict bindings = do
  env <- varEnv <$> getGlobalContext
  mapM_ (bindTo env) bindings
    where
      bindTo :: JSEnv -> (Ident, Expr) -> Runtime ()
      bindTo env (name, FunExpr _ paramList strictFun body) = bindFunc env name paramList strictFun body
      bindTo env (name, LiteralUndefined) = bindVar env name
      bindTo env other = error $ "Cannot bind " ++ show other

      bindFunc :: JSEnv -> Ident -> [Ident] -> Strictness -> [Statement] -> Runtime ()
      bindFunc env fn paramList strictFun body = do -- (5)
        fo <- createFunction (Just fn) paramList strictFun body env -- (5b)
        envRec <- envRec <$> deref env
        funcAlreadyDeclared <- hasBinding fn envRec -- (5c)

        if not funcAlreadyDeclared
        then createMutableBinding fn False envRec
        else do
          go <- getGlobalObject
          when (isTopLevelEnvRec envRec go) $ do
            Just existingProp <- objGetProperty fn go
            if propIsConfigurable existingProp
            then void $ defineOwnProperty fn blankDesc True go
            else when (propIsUnwritable existingProp) $
                  raiseTypeError $ "Cannot overwrite function " ++ fn

        setMutableBinding fn fo (strictFun == Strict) envRec


      bindVar :: JSEnv -> Ident -> Runtime ()
      bindVar env dn = do -- (8)
        envRec <- envRec <$> deref env
        varAlreadyDeclared <- hasBinding dn envRec
        unless varAlreadyDeclared $ do
          createMutableBinding dn False envRec
          setMutableBinding dn VUndef (strict == Strict) envRec

      isTopLevelEnvRec :: EnvRec -> Shared JSObj -> Bool
      isTopLevelEnvRec (ObjEnvRec o _) go = o == go
      isTopLevelEnvRec _ _ = False

      configurableBindings :: Bool
      configurableBindings = dbiType == DBIEval

      blankDesc :: PropDesc JSVal
      blankDesc = dataPD VUndef True True configurableBindings


firstArg :: JSVal -> [JSVal] -> JSVal
firstArg defaultVal [] = defaultVal
firstArg _ (x:xs) = x

first1 :: [JSVal] -> JSVal
first1 xs = a where [a] = take 1 (xs ++ repeat VUndef)

first2 :: [JSVal] -> (JSVal, JSVal)
first2 xs = (a,b) where [a,b] = take 2 (xs ++ repeat VUndef)

first3 :: [JSVal] -> (JSVal, JSVal, JSVal)
first3 xs = (a,b,c) where [a,b,c] = take 3 (xs ++ repeat VUndef)

tail1 :: [JSVal] -> [JSVal]
tail1 [] = []
tail1 (x:xs) = xs

addReadOnlyConstants :: [(String, JSNum)] -> Shared JSObj -> Runtime (Shared JSObj)
addReadOnlyConstants xs obj = do
  forM xs $ \(name, value) -> addOwnConstant name (VNum value) obj
  return obj
