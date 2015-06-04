{-# Language LambdaCase #-}

module Runtime (module Runtime, module X) where
import Control.Monad
import Control.Monad.State
import Control.Monad.Except
import Control.Monad.Writer
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
import JSNum as X
import Parse
import Expr
import JSNum

initialCxt :: Runtime JSCxt
initialCxt = do
  env <- initialEnv
  obj <- VObj <$> getGlobalObject
  return $ JSCxt env env obj NotStrict

initialEnv :: Runtime JSEnv
initialEnv = do
  global <- getGlobalObject
  share $ LexEnv (ObjEnvRec global False) Nothing

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
                         >>= addOwnPropertyDescriptor "length" (DataPD len True False False)
                         >>= setArrayIndices assigns
        return $ VObj obj

setArrayIndices :: [(Integer, JSVal)] -> Shared JSObj -> Runtime (Shared JSObj)
setArrayIndices assigns objRef = do
  mapM_ (\(n, v) -> objPut (show n) v False objRef) assigns
  return objRef


funFunction :: Shared JSObj -> JSFunction
funFunction prototype _this args = do
  obj <- newObject >>= setClass "Function"
                   >>= objSetPrototype prototype
  funConstructor (VObj obj) args

funConstructor :: JSVal -> [JSVal] -> Runtime JSVal
funConstructor this args = case this of
  VObj objRef -> do
    let arg = first1 args
    body <- toString arg
    let Program strictness stmts = simpleParseInFunction body
    constructFunction [] strictness stmts objRef
    return this
  _ -> raiseTypeError "Function constructor"


constructFunction :: [Ident] -> Strictness -> [Statement] -> Shared JSObj -> Runtime (Shared JSObj)
constructFunction paramList strict body this = do
  newProto <- newObject
  mkFunction "XXX" newProto this
    >>= setCstrMethod (funcCall paramList strict body)
    >>= setCallMethod (funcCall paramList strict body)


createFunction :: [Ident] -> Strictness -> [Statement] -> Runtime JSVal
createFunction paramList strict body = do
  prototype <- objFindPrototype "Function"
  VObj <$> (newObject >>= constructFunction paramList strict body
                      >>= objSetPrototype prototype
                      >>= addOwnProperty "length" (VNum $ fromIntegral $ length paramList))



-- ref 13.2.1
funcCall :: [Ident] -> Strictness -> [Statement] -> JSVal -> [JSVal] -> Runtime JSVal
funcCall paramList strict body this args =
  let makeRef env name = JSRef (VEnv env) name NotStrict
      newCxt cxt env newThis = cxt { thisBinding = newThis, lexEnv = env, varEnv = env, cxtStrictness = strict }
      addToNewEnv :: EnvRec -> Ident -> JSVal -> Runtime ()
      addToNewEnv env x v = putValue (makeRef env x) v
      findNewThis VNull  = getGlobalObject
      findNewThis VUndef = getGlobalObject
      findNewThis other  = toObject other

  in do
    cxt <- getGlobalContext
    rec <- newEnvRec
    env <- newEnv rec (lexEnv cxt)
    zipWithM_ (addToNewEnv rec) paramList args
    VObj arguments <- createArray (map Just args)
    addOwnProperty "callee" (VNum 42) arguments
    addToNewEnv rec "arguments" (VObj arguments)
    newThis <- VObj <$> findNewThis this
    withNewContext (newCxt cxt env newThis) $ do
      performDBI DBIFunction strict body
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
  where f (k, a) = (k, valueToProp a)

createGlobalObjectPrototype :: Runtime (Shared JSObj)
createGlobalObjectPrototype =
  let props = propsFromList [
                ("prototype", VUndef),
                ("toString", VNative objToString),
                ("hasOwnProperty", VNative objHasOwnProperty),
                ("valueOf", VNative objValueOf) ]
  in share $ emptyObject { ownProperties = props }


createGlobalThis :: Runtime (Shared JSObj)
createGlobalThis = do
  prototype <- getGlobalObjectPrototype

  functionPrototype <- newObject
    >>= setCallMethod (\_this _args -> return VUndef)
    >>= addOwnProperty "length" (VNum 0)

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
  addOwnProperty "constructor" (VObj object) object

  newObject
    >>= addOwnProperty "Object" (VObj object)
    >>= addOwnProperty "Function" (VObj function)
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
  putValue ref v


-- ref 10.2.2.1
getIdentifierReference :: Maybe JSEnv -> Ident -> Strictness -> Runtime JSRef
getIdentifierReference Nothing name strict = return $ JSRef VUndef name strict
getIdentifierReference (Just lexRef) name strict = do
  env <- deref lexRef
  exists <- hasBinding name (envRec env)
  if exists
  then return $ JSRef (VEnv $ envRec env) name strict
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
funCall :: JSVal -> [JSVal] -> Runtime JSVal
funCall ref argList = do
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
        prop <- objGetProperty "prototype" funref
        prototype <- fromObj prop
        setPrototype prototype obj
        objDefineOwnProperty "constructor" (DataPD val True False True) False obj
        objCstr (VObj funref) (VObj obj) args
        return obj
      _ -> raiseError $ "Can't invoke constructor " ++ name
    fromObj :: Maybe (PropDesc JSVal) -> Runtime (Maybe (Shared JSObj))
    fromObj Nothing = return Nothing
    fromObj (Just desc) = do
      val <- propValue desc fun -- XXX fun?
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

-- ref 10.5
data DBIType = DBIGlobal | DBIFunction | DBIEval
performDBI :: DBIType -> Strictness -> [Statement] -> Runtime ()
performDBI _dbiType strict stmts = do
  env <- varEnv <$> getGlobalContext
  mapM_ (bindVar env) (concatMap searchFunctionNames stmts)
  mapM_ (bindVar env) (concatMap searchVariables stmts)
    where
      bindVar :: JSEnv -> Ident -> Runtime ()
      bindVar env dn = do -- (8)
        rec <- envRec <$> deref env
        varAlreadyDeclared <- hasBinding dn rec
        unless varAlreadyDeclared $ do
          createMutableBinding dn False env
          setMutableBinding dn VUndef (strict == Strict) rec

searchFunctionNames :: Statement -> [Ident]
searchFunctionNames = walkStatement (const []) fnFinder

fnFinder :: Expr -> [Ident]
fnFinder (FunDef (Just fn) _ _ _) = [fn]
fnFinder _ = []

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

addReadOnlyConstants :: [(String, JSNum)] -> Shared JSObj -> Runtime (Shared JSObj)
addReadOnlyConstants xs obj = do
  forM xs $ \(name, value) -> addOwnConstant name (VNum value) obj
  return obj
