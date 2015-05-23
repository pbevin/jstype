module Runtime ( module Runtime.Types
               , module Runtime.Reference
               , module Runtime.Object
               , module Runtime.Operations
               , module Runtime.Conversion
               , getGlobalObject
               , emptyGlobal
               , module Runtime) where

import Control.Monad
import Control.Monad.State
import Control.Monad.Except
import Control.Monad.Writer
import Control.Applicative
import Data.Maybe
import qualified Data.Map as M
import Safe

import Runtime.Reference
import Runtime.Types
import Runtime.Object
import Runtime.Operations
import Runtime.Conversion
import Runtime.Global
import Parse
import Expr
import JSNum

import Debug.Trace

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

newObject :: JSRuntime (Shared JSObj)
newObject = do
  prototype <- share objectPrototype -- XXX
  share JSObj { objClass = "Object",
                ownProperties = M.fromList [("prototype", VObj prototype)],
                callMethod = Nothing,
                cstrMethod = Nothing,
                primitive = Nothing }

objectPrototype :: JSObj
objectPrototype =
  JSObj { objClass = "Object",
          ownProperties =
            M.fromList [ ("prototype", VUndef),
                         ("toString", VNative objToString),
                         ("prim", VNative objPrimitive) ],
          callMethod = Nothing,
          cstrMethod = Nothing,
          primitive = Nothing }

objToString :: JSFunction
objToString this _args = toString this >>= return . VStr

objPrimitive :: JSFunction
objPrimitive (VObj this) _args = do
  obj <- deref this
  objDefaultValue HintNone obj

toObject :: JSCxt -> JSVal -> JSRuntime JSVal
toObject _ v@(VObj obj) = return v
toObject cxt (VStr str) = do
  obj <- newObject
  stringConstructor (VObj obj) [VStr str]
  -- runExprStmt cxt (FunCall (NewExpr (ReadVar "String") [Str str]) [])
toObject cxt (VException (_, v, _)) = return v
toObject cxt v = toString v >>= toObject cxt . VStr


computeThisValue :: JSCxt -> JSVal -> JSVal
computeThisValue cxt v = case v of
  VRef ref ->
    if isPropertyReference ref
    then getBase ref
    else thisBinding cxt

  _ -> thisBinding cxt
createError :: JSVal -> JSRuntime JSVal
createError text =
  VObj <$> (newObject >>= setClass "Error"
                      >>= addOwnProperty "message" text)

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
        objCstr cxt (VObj funref) (VObj obj) args
        return obj
      _ -> raiseError $ "Can't invoke constructor " ++ name

-- ref 15.2.1.1
objFunction :: JSVal -> [JSVal] -> JSRuntime JSVal
objFunction this args =
  let arg = headDef VUndef args
  in if arg == VUndef || arg == VNull
     then VObj <$> newObject
     else initialCxt >>= flip toObject (head args)

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

createFunction :: [Ident] -> Strictness -> [Statement] -> JSCxt -> JSRuntime JSVal
createFunction paramList strict body cxt = do
  objref <- newObject
  modifyRef objref $
    \obj -> obj { objClass = "Function",
                  callMethod = Just (funcCall cxt paramList body),
                  cstrMethod = Just (funcCall cxt paramList body) }
  return $ VObj objref

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
    result <- jsRunStmts (newCxt env) body
    case result of
      (CTReturn, Just v, _) -> return v
      (CTThrow, Just (VException v), _) -> throwError v
      (CTThrow, Just v, _)  -> throwError ("", v, [])
      _ -> return VUndef

objCall :: JSCxt -> JSVal -> JSVal -> [JSVal] -> JSRuntime JSVal
objCall cxt func this args = case func of
  VNative f -> f this args
  VObj objref -> deref objref >>= \obj -> case callMethod obj of
    Nothing -> raiseError "Can't call function: no callMethod"
    Just method -> method this args
  VUndef -> raiseError "Undefined function"
  _ -> raiseError $ "Can't call " ++ show func

objCstr :: JSCxt -> JSVal -> JSVal -> [JSVal] -> JSRuntime JSVal
objCstr cxt func this args = case func of
  VNative f -> f this args
  VObj objref -> deref objref >>= \obj -> case cstrMethod obj of
    Nothing -> raiseError "Can't call function: no cstrMethod"
    Just method -> method this args
  VUndef -> raiseError "Undefined function"
  _ -> raiseError $ "Can't call " ++ show func


-- ref 15.1.2.1
objEval :: JSFunction
objEval _this args = case args of
  [] -> return VUndef
  (prog:_) -> do
    text <- toString prog
    result <- jsEvalCode text
    case result of
      (CTNormal, Just v, _) -> return v
      (CTThrow, Just (VException v), _) -> throwError v
      (CTThrow, Just v, _)  -> throwError ("", v, [])
      _ -> return VUndef

stackTrace :: JSError -> String
stackTrace (excType, err, trace) = unlines $ show err : map show (reverse trace)


printStackTrace :: JSError -> JSRuntime ()
printStackTrace = liftIO . putStrLn . stackTrace

createGlobalThis :: JSRuntime (Shared JSObj)
createGlobalThis = do
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
  error <- newObject >>= setCallMethod errConstructor
                     >>= setCstrMethod errConstructor

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
            >>= addOwnProperty "Error" (VObj error)
            >>= addOwnProperty "ReferenceError" (VObj error)
            >>= addOwnProperty "Math" (VObj math)
            >>= addOwnProperty "undefined" (VUndef)
            >>= addOwnProperty "null" (VNull)
            >>= addOwnProperty "eval" (VNative objEval)
            >>= addOwnProperty "Infinity" (VNum $ 1 / 0)
            >>= addOwnProperty "NaN" (VNum $ jsNaN)
            >>= addOwnProperty "isNaN" (VNative objIsNaN)


-- ref B.2.1, incomplete
objEscape :: JSFunction
objEscape _this args = case args of
  [] -> return VUndef
  (x:xs) -> return x

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

