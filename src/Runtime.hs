{-# Language LambdaCase, OverloadedStrings #-}

module Runtime (module Runtime, module X) where

import Control.Lens hiding (strict, Getter, Setter)
import Control.Monad
import Control.Monad.State
import Control.Monad.Except
import Control.Monad.Fix
import Control.Applicative
import Control.Arrow
import Text.Printf
import Data.Maybe
import Data.Monoid
import qualified Data.Text as T
import Data.Text (Text)
import Safe

import Runtime.Reference as X
import Runtime.Types as X
import Runtime.Object as X
import Runtime.Operations as X
import Runtime.Conversion as X
import Runtime.Global as X
import Runtime.Shared as X
import Runtime.Error as X
import Runtime.Arguments as X
import Runtime.PropMap as X
import Runtime.PropDesc as X
import Runtime.Function as X
import Runtime.Prototype as X
import Runtime.PropertyDescriptor as X
import Runtime.ObjectBuilder as X
import Unicode as X
import Parse as X
import Expr (Program(..), Strictness(..), Expr(..), Statement, Ident, SrcLoc)
import Core

import Debug.Trace

buildGlobalObject :: Runtime ()
buildGlobalObject = do
  objectPrototype <- getGlobalObjectPrototype

  updateObject objectPrototype $ do
    property "prototype" VUndef
    method "toString" 0 objToString
    method "hasOwnProperty" 1 objHasOwnProperty
    method "valueOf" 0 objValueOf
    method "isPrototypeOf" 1 objIsPrototypeOf
    method "propertyIsEnumerable" 1 objPropertyIsEnumerable

  functionPrototype <- mkObject $ do
    internal callMethod (\_this _args -> return VUndef)
    property "length" (VInt 0)
    method "call" 1 funCallMethod
    method "bind" 1 funBind
    method "apply" 2 funApply

  function <- mkObject $ do
    prototype functionPrototype
    className "Function"
    property "prototype" (VObj functionPrototype)
    property "name" (VStr "Function")
    property "length" (VInt 1) -- ref 15.3.3.2
    internal callMethod $ funFunction functionPrototype
    internal cstrMethod funConstructor
    internal hasInstanceMethod funHasInstance

  object <- mkObject $ do
    className "Function"
    property "prototype" (VObj objectPrototype)
    internal hasInstanceMethod funHasInstance
    internal callMethod objFunction
    internal cstrMethod objConstructor
    method "getOwnPropertyDescriptor" 2 getOwnPropertyDescriptor
    method "getOwnPropertyNames"      1 getOwnPropertyNames
    method "defineProperty"           3 objDefineProperty
    method "preventExtensions"        1 objPreventExtensions
    method "getPrototypeOf"           1 objGetPrototypeOf

  addOwnProperty "prototype" (VObj objectPrototype) object
  addOwnProperty "constructor" (VObj object) objectPrototype

  obj <- getGlobalObject
  updateObject obj $ do
    property "prototype" (VObj objectPrototype)
    property "Object" (VObj object)
    property "Function" (VObj function)

initialCxt :: Runtime JSCxt
initialCxt = do
  env <- initialEnv
  obj <- VObj <$> getGlobalObject
  return $ JSCxt env env obj NotStrict
  where
    initialEnv :: Runtime JSEnv
    initialEnv = do
      global <- getGlobalObject
      shareLexEnv $ LexEnv (ObjEnvRec global False) Nothing

stringifyException :: JSVal -> Runtime JSError
stringifyException v = do
  msg <- toString v
  st <- getStackTrace v
  return $ JSError (VStr msg, st)

exceptionToVal :: ([SrcLoc] -> [SrcLoc]) -> JSError -> Runtime JSVal
exceptionToVal f (JSError (err, stack)) = do
  setStacktrace err (f stack)
  return err
exceptionToVal f (JSProtoError (t, msg)) = do
  err <- createError t (VStr $ T.pack msg)
  exceptionToVal f (JSError (err, []))

setStacktrace :: JSVal -> [SrcLoc] -> Runtime ()
setStacktrace v stack =
  case v of
    VObj objRef -> void $ addOwnProperty "stack" (VStacktrace stack) objRef
    _           -> return ()

-- ref 15.2.4.2
objToString :: JSFunction
objToString this _args =
  case this of
    VUndef -> return . VStr $ "[object Undefined]"
    VNull  -> return . VStr $ "[object Null]"
    _ -> do
      o   <- toObject this
      cls <- view objClass <$> deref o
      return . VStr $ "[object " <> cls <> "]"

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

-- ref 15.2.2.1
objConstructor :: JSVal -> [JSVal] -> Runtime JSVal
objConstructor _this args = let v = first1 args in do
  case v of
    VObj _ -> return v
    VNull  -> makeNew
    VUndef -> makeNew
    _      -> VObj <$> toObject v
  where
    makeNew = do
      proto <- objFindPrototype "Object"
      obj <- mkObject $ do
        className "Object"
        prototype proto
        extensible
      return $ VObj obj

numConstructor :: JSVal -> [JSVal] -> Runtime JSVal
numConstructor this args = do
  num <- case headMay args of
    Nothing       -> return (VInt 0)
    Just (VInt n) -> return (VInt n)
    Just (VNum n) -> return (VNum n)
    Just other    -> VNum <$> toNumber other
  case this of
    VObj obj -> do
      VObj <$> (setClass "Number" obj >>= objSetPrimitive num)
    _ -> raiseError . T.pack $ "numConstructor called with this = " ++ show this


arrayAssigns :: [Maybe a] -> [(Int, a)]
arrayAssigns xs = map (second fromJust) $ filter (isJust.snd) $ zip [0..] xs

createArray :: [Maybe JSVal] -> Runtime JSVal
createArray vals = if all isJust vals
                      then createDenseArray (catMaybes vals)
                      else createSparseArray (length vals) (arrayAssigns vals)

createDenseArray :: [JSVal] -> Runtime JSVal
createDenseArray vals =
  -- TODO: new dense array type
  createSparseArray (length vals) (zip [0..] vals)

createSparseArray :: Int -> [(Int, JSVal)] -> Runtime JSVal
createSparseArray len assigns = do
  cstr <- getGlobalProperty "Array"
  arrayPrototype <- objFindPrototype "Array"
  obj <- mkObject $ do
    className "Array"
    prototype arrayPrototype
    descriptor "constructor" (dataPD cstr True False  True)
    descriptor "length"      (dataPD (VInt $ fromIntegral len)  True False False)

  setArrayIndices assigns obj
  return $ VObj obj


-- ref 11.1.5
createObjectLiteral :: [(Text, JSVal)] -> Runtime JSVal
createObjectLiteral nameValueList =do
  cstr <- getGlobalProperty "Object"
  obj <- newObject >>= addOwnProperty "constructor" cstr
  mapM_ (addObjectProp obj) nameValueList
  return (VObj obj)

  where
    addObjectProp :: Shared JSObj -> (Text, JSVal) -> Runtime (Shared JSObj)
    addObjectProp obj (name, value) = do
      desc <- makeDescriptor value
      objGetOwnProperty name obj >>= \case
        Just previous -> checkCompatible previous desc
        Nothing -> return ()
      defineOwnProperty name desc False obj
      return obj

    makeDescriptor :: JSVal -> Runtime (PropDesc JSVal)
    makeDescriptor val = case val of
      VGetter body    -> makeGetter body
      VSetter v body  -> makeSetter v body
      _               -> return $ dataPD val True True True

    makeGetter :: [Statement] -> Runtime (PropDesc JSVal)
    makeGetter body = do
      strict <- getGlobalStrictness
      env <- lexEnv <$> getGlobalContext
      func <- createFunction Nothing [] strict body env >>= mkGetter
      return $ accessorPD func Nothing True True

    makeSetter :: Ident -> [Statement] -> Runtime (PropDesc JSVal)
    makeSetter param body = do
      strict <- getGlobalStrictness
      env <- lexEnv <$> getGlobalContext
      func <- createFunction Nothing [param] strict body env >>= mkSetter
      return $ accessorPD Nothing func True True

    checkCompatible :: PropDesc JSVal -> PropDesc JSVal -> Runtime ()
    checkCompatible a b = do
      let failure = raiseSyntaxError "Cannot reassign property"
      strict <- (== Strict) <$> getGlobalStrictness
      when (strict && isDataDescriptor (Just a) && isDataDescriptor (Just b)) failure
      when (isDataDescriptor (Just a) && isAccessorDescriptor (Just b)) failure
      when (isAccessorDescriptor (Just a) && isDataDescriptor (Just b)) failure
      when (isAccessorDescriptor (Just a) && isAccessorDescriptor (Just b)) $ do
        when (hasGetter a && hasGetter b) failure
        when (hasSetter a && hasSetter b) failure
      return ()



setArrayIndices :: [(Int, JSVal)] -> Shared JSObj -> Runtime (Shared JSObj)
setArrayIndices assigns objRef = do
  mapM_ (\(n, v) -> defineOwnProperty (T.pack $ show n) (dataPD v True True True) False objRef) assigns
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
      Left err -> raiseSyntaxError (T.pack $ show err)
      Right (Program strictness stmts) -> do
        globalEnv <- getGlobalEnvironment
        createFunction Nothing params strictness stmts globalEnv
  _ -> raiseTypeError "Function constructor"

funCallMethod :: JSFunction
funCallMethod this args = do
  isCallable this >>= \case
    Nothing   -> raiseTypeError . T.pack $ "Not a function: " ++ show this
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
      args <- forM [0..n] $ \index -> objGet (T.pack $ show index) argArray
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

      targetCstr   <- view cstrMethod <$> deref target
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
                     >>= addOwnPropertyDescriptor "length" (dataPD (VInt $ fromIntegral l) False False False)
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
      nparams = VInt $ fromIntegral $ length paramList
      prototypeProperty prototype = dataPD prototype True False False
      nameProperty = fromMaybe "" name
      callMethod func = funcCall name func paramList strict body

throwerProperty :: PropDesc JSVal
throwerProperty = accessorPD (Just thrower) (Just (const thrower)) False False

thrower :: a -> Runtime b
thrower _ = raiseTypeError "Cannot access property"

-- ref 15.3.5.4
funGet :: Text -> Shared JSObj -> Runtime JSVal
funGet p f = do
  val <- objGetObj p f
  if p /= "caller"
  then return val
  else case val of
    VObj obj -> do
      prog <- view objCode <$> deref obj
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

    scope <- view objScope <$> deref (toObj func)
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
        VObj argsObj <- createArgumentsObject func (map T.unpack paramList) args env strict
        addToNewEnv env "arguments" (VObj argsObj)
      result <- jsRunStmts body
      case result of
        Left  err -> rethrowWithStack err
        Right val -> return val

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
jsConsoleLog _this xs = tell (T.unwords (map showVal xs) <> "\n") >> return VUndef

tell :: Text -> Runtime ()
tell msg = output <>= msg

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
  envRec <- sharePropertyMap emptyPropMap
  shareLexEnv $ LexEnv (DeclEnvRec envRec) e


-- ref 10.2.2.3
newObjectEnvironment :: Shared JSObj -> Maybe JSEnv -> Bool -> Runtime JSEnv
newObjectEnvironment obj oldEnv provideThis = shareLexEnv $ LexEnv (ObjEnvRec obj provideThis) oldEnv


-- ref 11.13.1
assignRef :: JSVal -> JSVal -> Runtime JSVal
assignRef lref rref =
  case lref of
    VRef ref -> do
      rval <- getValue rref
      disallowEvalAssignment ref
      putValue ref rval
      return rval
    _ -> raiseReferenceError . T.pack $ show lref ++ " is not assignable"

updateRef :: String -> JSVal -> JSVal -> Runtime JSVal
updateRef op lref rref =
  case lref of
    VRef ref -> do
      lval   <- getValue lref
      rval   <- getValue rref
      newVal <- evalBinOp (T.pack op) lval rval
      disallowEvalAssignment ref
      putValue ref newVal
      return newVal
    _ -> raiseReferenceError . T.pack $ show lref ++ " is not assignable"

-- ref 11.13.1
disallowEvalAssignment :: JSRef -> Runtime ()
disallowEvalAssignment (JSRef (VEnv _) name strict)
  | name /= "eval" && name /= "arguments" = return ()
  | strict == NotStrict                   = return ()
  | otherwise = cannotAssignTo name
disallowEvalAssignment x = return ()

cannotAssignTo :: Text -> Runtime ()
cannotAssignTo name = raiseSyntaxError $ "Assignment of " <> name <> " in strict mode"

-- ref. 11.2.1
memberGet :: JSVal -> String -> Runtime JSVal
memberGet lval prop = do
  strict <- getGlobalStrictness
  return $ VRef (JSRef lval (T.pack prop) strict)

-- ref 11.2.3
callFunction :: JSVal -> [JSVal] -> Runtime JSVal
callFunction ref argList = do
  cxt <- getGlobalContext
  func <- getValue ref
  thisValue <- computeThisValue cxt ref
  case evalCallType ref func of
    DirectEvalCall -> objEval DirectEvalCall thisValue argList
    _              -> do
      assertFunction name (^.callMethod) func
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

assertFunction :: Text -> (JSObj -> Maybe a) -> JSVal -> Runtime ()
assertFunction name m val =
  case val of
    VNative _ _ _ -> return ()
    VObj o -> do
      m <$> deref o >>= \case
        Just _ -> return ()
        Nothing -> error "is not a function"
    VUndef -> error "is undefined"
    _ -> error "is not a function"

  where error reason = raiseTypeError $ T.unwords [name, reason]

-- ref 15.1.2.1
objEval :: EvalCallType -> JSFunction
objEval callType _this args = case args of
  [] -> return VUndef
  (prog:_) -> do
    text <- toString prog
    jsEvalCode callType text

-- ref 15.1.2.1.1
evalCallType :: JSVal -> JSVal -> EvalCallType
evalCallType (VRef (JSRef (VEnv env) "eval" _)) (VNative "eval" _ _) = DirectEvalCall
evalCallType _ _                                                     = IndirectEvalCall

-- ref 15.1.2.4
objIsNaN :: JSFunction
objIsNaN _this args =
  let arg = first1 args
  in VBool . isNaN <$> toNumber arg

-- ref 13.2.2, incomplete
newObjectFromConstructor :: JSVal -> [JSVal] -> Runtime (Shared JSObj)
newObjectFromConstructor fun args = case fun of
  VRef (JSRef _ name _) -> create name =<< getValue fun
  _                     -> create (T.pack $ show fun) fun
  where
    create :: Text -> JSVal -> Runtime (Shared JSObj)
    create name val = case val of
      VObj funref -> do
        obj <- newObject
        prototype <- objGetProperty "prototype" funref >>= fromObj
        case prototype of
          Just p -> setPrototype prototype obj
          Nothing -> do
            objPrototype <- Just <$> getGlobalObjectPrototype
            setPrototype objPrototype obj
        -- defineOwnProperty "constructor" (dataPD val True False True) False obj
        objCstr (VObj funref) (VObj obj) args >>= \case
          VObj o -> return o
          _ -> return obj
      _ -> raiseProtoError TypeError . T.unpack $ "Can't invoke constructor " <> name
    fromObj :: Maybe (PropDesc JSVal) -> Runtime (Maybe (Shared JSObj))
    fromObj Nothing = return Nothing
    fromObj (Just desc) = do
      val <- return $ fromMaybe VUndef $ propValue desc
      case val of
        VObj obj -> return $ Just obj
        _        -> return Nothing
    setPrototype :: Maybe (Shared JSObj) -> Shared JSObj -> Runtime (Shared JSObj)
    setPrototype mp = updateObj $ set objPrototype mp

objCall :: JSVal -> JSVal -> [JSVal] -> Runtime JSVal
objCall func this args = case func of
  VNative _ _ f -> f this args
  VObj objref -> view callMethod <$> deref objref >>= \case
    Nothing -> raiseError "Can't call function: no callMethod"
    Just method -> method this args
  VUndef -> raiseReferenceError "Function is undefined"
  _ -> raiseError . T.pack $ "Can't call " ++ show func

objCstr :: JSVal -> JSVal -> [JSVal] -> Runtime JSVal
objCstr func this args = case func of
  VNative _ _ f -> f this args
  VObj objref -> view cstrMethod <$> deref objref >>= \case
    Nothing -> raiseError "Can't create object: function has no cstrMethod"
    Just method -> method this args
  VUndef -> raiseError "Undefined function"
  _ -> raiseError . T.pack $ "Can't call " ++ show func


-- ref 15.2.3.2
objGetPrototypeOf :: JSFunction
objGetPrototypeOf _this args =
  let o = first1 args
  in case o of
    VObj obj -> maybe VUndef VObj . view objPrototype <$> deref obj
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
      ks <- propMapKeys . view ownProperties <$> deref obj
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
          v' <- view objPrototype <$> deref v
          findPrototype o v'
  in do
    case arg of
      VObj v -> do
        o <- toObject this
        v' <- view objPrototype <$> deref v
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
objId (VObj obj) _args = return $ VInt $ fromIntegral (objid obj)
objId x _ = raiseTypeError . T.pack $ "No objId for " ++ show x

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
                  raiseTypeError $ "Cannot overwrite function " <> fn

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

addReadOnlyConstants :: [(Text, Double)] -> Shared JSObj -> Runtime (Shared JSObj)
addReadOnlyConstants xs obj = do
  forM xs $ \(name, value) -> addOwnConstant name (VNum value) obj
  return obj
