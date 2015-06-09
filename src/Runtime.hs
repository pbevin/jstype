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
import Runtime.PropMap as X
import Runtime.PropDesc as X
import Runtime.Function as X
import Runtime.Prototype as X
import Runtime.PropertyDescriptor as X
import JSNum as X
import Parse
import Expr
import JSNum

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
            >>= addOwnProperty "toString" (VNative objToString)
            >>= addOwnProperty "hasOwnProperty" (VNative objHasOwnProperty)
            >>= addOwnProperty "valueOf" (VNative objValueOf)
            >>= addOwnProperty "__objid" (VNative objId)

createGlobalThis :: Runtime (Shared JSObj)
createGlobalThis = do
  prototype <- getGlobalObjectPrototype

  functionPrototype <- newObject
    >>= setCallMethod (\_this _args -> return VUndef)
    >>= addOwnProperty "length" (VNum 0)
    >>= addOwnProperty "call" (VNative funCallMethod)

  function <- newObject
    >>= mkFunction "Function" functionPrototype
    >>= setCallMethod (funFunction functionPrototype)
    >>= setCstrMethod funConstructor
    >>= objSetPrototype functionPrototype
    >>= addOwnProperty "prototype" (VObj functionPrototype)
    >>= addOwnConstant "length" (VNum 1) -- ref 15.3.3.2

  object <- functionObject "Object" prototype
    >>= addOwnProperty "getOwnPropertyDescriptor" (VNative getOwnPropertyDescriptor)
    >>= addOwnProperty "getOwnPropertyNames" (VNative getOwnPropertyNames)
    >>= addOwnProperty "defineProperty" (VNative objDefineProperty)
    >>= addOwnProperty "preventExtensions" (VNative objPreventExtensions)
    >>= setCallMethod objFunction
    >>= setCstrMethod objConstructor
  addOwnProperty "prototype" (VObj prototype) object
  addOwnProperty "constructor" (VObj object) prototype

  newObject
    >>= addOwnProperty "Object" (VObj object)
    >>= addOwnProperty "Function" (VObj function)

objToString :: JSFunction
objToString _this _args = return $ VStr "[object Object]"

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
                         >>= addOwnProperty "constructor" cstr
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

funConstructor :: JSVal -> [JSVal] -> Runtime JSVal
funConstructor this args = case this of
  VObj objRef -> do
    let arg = lastDef VUndef args
    body <- toString arg
    case parseInFunction body of
      Left err -> raiseSyntaxError (show err)
      Right (Program strictness stmts) -> do
        globalEnv <- getGlobalEnvironment
        createFunction Nothing [] strictness stmts globalEnv
  _ -> raiseTypeError "Function constructor"

funCallMethod :: JSFunction
funCallMethod this args = do
  isCallable this >>= \case
    Nothing   -> raiseTypeError $ "Not a function: " ++ show this
    Just call -> call (first1 args) (tail1 args)

-- ref 13.2
createFunction :: Maybe Ident -> [Ident] -> Strictness -> [Statement] -> Shared LexEnv -> Runtime JSVal
createFunction name paramList strict body scope =
  VObj <$> buildFunction
    where
      buildFunction :: Runtime (Shared JSObj)
      buildFunction = do
        functionPrototype <- objFindPrototype "Function"
        prototype <- VObj <$> newObject

        func <- newObject
          >>= setClass "Function" -- (3)
          >>= objSetPrototype functionPrototype -- (4)
          >>= setGetMethod funGet -- (5)
          >>= objSetHasInstance (funHasInstance) -- (8)
          >>= setScope scope
          >>= setFormalParameters paramList
          >>= setCode (Program strict body)
          >>= objSetExtensible True
          >>= defineOwnProperty "length" lengthProperty False
          >>= defineOwnProperty "prototype" (prototypeProperty prototype) False

        setCallMethod (callMethod $ VObj func) func -- (6) XXX
        setCstrMethod (callMethod $ VObj func) func -- (7) XXX

        ifStrictContext $ do
          let prop = accessorPD (Just thrower) (Just thrower) False False
          defineOwnProperty "caller" prop False func
          defineOwnProperty "arguments" prop False func

        return func

      lengthProperty = dataPD nparams False False True
      nparams = VNum $ fromIntegral $ length paramList
      prototypeProperty prototype = dataPD prototype True False False
      nameProperty = fromMaybe "" name
      callMethod func = funcCall name func paramList strict body
      thrower _ = raiseTypeError "Cannot access property"

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



funHasInstance = undefined



-- ref 13.2.1
funcCall :: Maybe Ident -> JSVal -> [Ident] -> Strictness -> [Statement] -> JSVal -> [JSVal] -> Runtime JSVal
funcCall name func paramList strict body this args =
  let makeRef env name = JSRef (VEnv env) name NotStrict
      newCxt cxt env newThis = cxt { thisBinding = newThis, lexEnv = env, varEnv = env, cxtStrictness = strict }
      addToNewEnv :: EnvRec -> Ident -> JSVal -> Runtime ()
      addToNewEnv env x v = putValue (makeRef env x) v
      findNewThis VNull  = getGlobalObject
      findNewThis VUndef = getGlobalObject
      findNewThis other  = toObject other
  in do
    cxt <- getGlobalContext

    scope <- objScope <$> deref (toObj func)
    localEnv <- newDeclarativeEnvironment scope
    env <- envRec <$> deref localEnv
    zipWithM_ (addToNewEnv env) paramList (args ++ repeat VUndef)
    VObj arguments <- createArgumentsObject func paramList args strict
    addToNewEnv env "arguments" (VObj arguments)
    case name of
      Just n -> addToNewEnv env n func
      _      -> return ()
    newThis <- VObj <$> findNewThis this
    withNewContext (newCxt cxt localEnv newThis) $ do
      performDBI DBIFunction strict body
      result <- jsRunStmts body
      case result of
        (CTReturn, Just v, _) -> return v
        (CTThrow, Just v, _)  -> rethrowWithStack v
        _ -> return VUndef

rethrowWithStack :: JSVal -> Runtime a
rethrowWithStack v = do
  stack <- case v of
    VObj obj -> objGet "stack" obj >>= \case
                VStacktrace st -> return st
                _              -> return []
    _ -> return []
  throwError $ JSError (v, stack)

-- ref 10.6
createArgumentsObject :: JSVal -> [String] -> [JSVal] -> Strictness -> Runtime JSVal
createArgumentsObject func names args strict =
  let len = JSNum (fromIntegral $ length args)
      thrower _ = raiseTypeError "Cannot access property"
  in do
    object <- getGlobalProperty "Object"
    objectPrototype <- getGlobalObjectPrototype

    map <- newObject
    forM_ (reverse $ zip3 (names ++ repeat "") args [0..]) $ \(name, val, indx) -> do
      -- XXX incomplete step 11
      defineOwnProperty (show indx) (dataPD val True True True) False map

    cs <- objGet "constructor" objectPrototype

    obj <- newObject >>= setClass "Arguments"
                     >>= objSetPrototype objectPrototype
                     >>= defineOwnProperty "length" (dataPD (VNum len) True False True) False
                     >>= objSetParameterMap (VObj map) -- XXX missing step 12b

    -- Hack because I'm not willing to define new [[get]] etc. methods as per step 12b
    -- just now...
    forM_ (reverse $ zip3 (names ++ repeat "") args [0..]) $ \(name, val, indx) -> do
      -- XXX incomplete step 11
      defineOwnProperty (show indx) (dataPD val True True True) False obj

    case strict of
      NotStrict -> defineOwnProperty "callee" (dataPD func True False True) False obj
      Strict -> do
        defineOwnProperty "caller" (accessorPD (Just thrower) (Just thrower) False False) False obj
        defineOwnProperty "callee" (accessorPD (Just thrower) (Just thrower) False False) False obj

    return (VObj obj)



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
  else do
    getIdentifierReference (outer env) name strict

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
  assertFunction name callMethod func
  thisValue <- computeThisValue cxt ref
  objCall func thisValue argList

  where
    computeThisValue cxt v = case v of
      VRef ref ->
        if isPropertyReference ref
        then return (getBase ref)
        else case getBase ref of
          VEnv env -> return (implicitThisValue env)

      _ -> return $ thisBinding cxt

    implicitThisValue (DeclEnvRec _) = VUndef
    implicitThisValue (ObjEnvRec obj True) = (VObj obj)
    implicitThisValue (ObjEnvRec _ False) = VUndef

    name = case ref of
      VRef (JSRef _ name _) -> name
      _ -> ""

assertFunction :: String -> (JSObj -> Maybe a) -> JSVal -> Runtime ()
assertFunction name m val =
  case val of
    VNative _ -> return ()
    VObj o -> do
      m <$> deref o >>= \case
        Just _ -> return ()
        Nothing -> error "is not a function"
    VUndef -> error "is undefined"
    _ -> error "is not a function"

  where error reason = raiseTypeError $ unwords [name, reason]

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
        setPrototype prototype obj
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
  VNative f -> f this args
  VObj objref -> deref objref >>= \obj -> case callMethod obj of
    Nothing -> raiseError "Can't call function: no callMethod"
    Just method -> method this args
  VUndef -> raiseReferenceError "Function is undefined"
  _ -> raiseError $ "Can't call " ++ show func

objCstr :: JSVal -> JSVal -> [JSVal] -> Runtime JSVal
objCstr func this args = case func of
  VNative f -> f this args
  VObj objref -> deref objref >>= \obj -> case cstrMethod obj of
    Nothing -> raiseError "Can't create object: function has no cstrMethod"
    Just method -> method this args
  VUndef -> raiseError "Undefined function"
  _ -> raiseError $ "Can't call " ++ show func

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

objId :: JSVal -> [JSVal] -> Runtime JSVal
objId (VObj (Shared _ oid)) _args = return $ VNum $ fromIntegral oid

-- ref 15.2.4.5
objHasOwnProperty :: JSVal -> [JSVal] -> Runtime JSVal
objHasOwnProperty this args =
  let v = first1 args
  in do
    p <- toString v
    o <- toObject this
    VBool . isJust <$> objGetOwnProperty p o

-- ref 10.5
data DBIType = DBIGlobal | DBIFunction | DBIEval deriving (Show, Eq)
performDBI :: DBIType -> Strictness -> [Statement] -> Runtime ()
performDBI dbiType strict stmts = do
  env <- varEnv <$> getGlobalContext
  mapM_ (bindVar env) (concatMap searchVariables stmts)
  mapM_ (bindFunc env) (concatMap searchFunctionNames stmts)
    where
      bindFunc :: JSEnv -> Statement -> Runtime ()
      bindFunc env (FunDecl _ fn paramList strict body) = do -- (5)
        fo <- createFunction (Just fn) paramList strict body env -- (5b)
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

        setMutableBinding fn fo (strict == Strict) envRec


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



searchFunctionNames :: Statement -> [Statement]
searchFunctionNames = walkStatement fnFinder (const [])

fnFinder :: Statement -> [Statement]
fnFinder stmt = case stmt of
  FunDecl _ fn _ _ _ -> [stmt]
  _                  -> []

searchVariables :: Statement -> [Ident]
searchVariables = walkStatement varFinder (const [])

varFinder :: Statement -> [Ident]
varFinder (VarDecl _ ds) = map fst ds
varFinder _ = []

walkStatement :: (Statement -> [a]) -> (Expr -> [a]) -> Statement -> [a]
walkStatement sv ev = walk where
  walk stmt = sv stmt ++ case stmt of
    Block _ ss               -> concatMap walk ss
    VarDecl _ vds -> concatMap ewalk (catMaybes $ map snd vds)
    ExprStmt _ e -> ewalk e
    LabelledStatement _ _ s  -> walk s
    IfStatement _ e s1 Nothing -> ewalk e ++ walk s1
    IfStatement _ e s1 (Just s2) -> ewalk e ++ walk s1 ++ walk s2
    WhileStatement _ e s     -> ewalk e ++ walk s
    DoWhileStatement _ e s   -> ewalk e ++ walk s
    Return _ (Just e)        -> ewalk e
    WithStatement _ e s      -> ewalk e ++ walk s
    For l h s                -> hwalk l h ++ walk s
    TryStatement _ s c f     -> walk s ++ concatMap walk (catMaybes [c, f])
    Catch _ _ s              -> walk s
    Finally _ s              -> walk s

    _                        -> []
  ewalk = walkExpr sv ev
  hwalk l header = case header of
    For3 e1 e2 e3            -> concatMap ewalk (catMaybes [e1, e2, e3])
    For3Var decls e2 e3      -> walk (VarDecl l decls) ++ concatMap ewalk (catMaybes [e2, e3])
    ForIn e1 e2              -> ewalk e1 ++ ewalk e2
    ForInVar decl e          -> walk (VarDecl l [decl]) ++ ewalk e

walkExpr :: (Statement -> [a]) -> (Expr -> [a]) -> Expr -> [a]
walkExpr sv ev = walk where
  walk expr = ev expr ++ case expr of
    ArrayLiteral es  -> concatMap walk (catMaybes es)
    ObjectLiteral as -> concatMap walkPropAss as
    BinOp _ e1 e2    -> walk e1 ++ walk e2
    UnOp _ e         -> walk e
    PostOp _ e       -> walk e
    NewExpr e es     -> walk e ++ concatMap walk es
    Assign e1 _ e2   -> walk e1 ++ walk e2
    Cond e1 e2 e3    -> walk e1 ++ walk e2 ++ walk e3
    MemberDot e _    -> walk e
    MemberGet e1 e2  -> walk e1 ++ walk e2
    FunCall e es     -> walk e ++ concatMap walk es
    _                -> []

  walkPropAss (_, Value e) = walk e
  walkPropAss (_, _) = []

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
