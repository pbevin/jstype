module Runtime.Operations where

import Control.Monad
import Control.Monad.Trans
import Control.Applicative
import Data.Fixed (mod')
import Data.Word
import Data.Bits
import Data.Maybe
import Expr
import JSNum
import Runtime.Types
import Runtime.Conversion
import Runtime.Object
import Runtime.PropMap

evalBinOp :: String -> JSVal -> JSVal -> Runtime JSVal
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
  "%"          -> numberOp $ fmod
  "<"          -> lessThan                -- ref 11.8.1
  ">"          -> flip (lessThan)         -- ref 11.8.2
  "<="         -> flip (invert lessThan)  -- ref 11.8.3
  ">="         -> invert lessThan         -- ref 11.8.4
  "&"          -> bitwise (.&.)           -- ref 11.10
  "|"          -> bitwise (.|.)           -- ref 11.10
  "^"          -> bitwise xor             -- ref 11.10
  "<<"         -> lshift
  ">>"         -> rshift
  ">>>"        -> urshift
  ","          -> commaOperator
  _            -> noSuchBinop op
  where invert f x y = unaryNot =<< f x y

noSuchBinop :: String -> JSVal -> JSVal -> Runtime JSVal
noSuchBinop op a b = raiseError $
  "No binop `" ++ op ++ "' on " ++ show (a, b)



-- ref 11.6.1, incomplete
jsAdd :: JSVal -> JSVal -> Runtime JSVal
jsAdd a b = do
  a' <- toPrimitive HintNone a
  b' <- toPrimitive HintNone b
  if isString a' || isString b'
  then VStr <$> liftStr (++) a' b'
  else VNum <$> liftNum (+) a' b'

-- ref 11.8.5
lessThan :: JSVal -> JSVal -> Runtime JSVal
lessThan a b = do
  a' <- toPrimitive HintNumber a
  b' <- toPrimitive HintNumber b
  liftM VBool $ if isString a' && isString b'
  then liftStr (<) a' b'
  else liftNum (<) a' b'

numberOp :: (JSNum->JSNum->JSNum) -> JSVal -> JSVal -> Runtime JSVal
numberOp op a b = do
  a' <- toPrimitive HintNone a
  b' <- toPrimitive HintNone b
  VNum <$> liftNum op a' b'

fmod :: JSNum -> JSNum -> JSNum
fmod n d
  | d == 0     = 0/0
  | infinite n = 0/0
  | infinite d = d
  | n >= 0     = mod' n d
  | n < 0      = -mod' (-n) d
  where infinite (JSNum x) = isInfinite x

boolOp :: (JSVal->JSVal->Bool) -> JSVal -> JSVal -> Runtime JSVal
boolOp op a b = return (VBool $ op a b)

liftNum :: (JSNum -> JSNum -> a) -> JSVal -> JSVal -> Runtime a
liftNum op a b = do
  n1 <- toNumber a
  n2 <- toNumber b
  return $ n1 `op` n2

liftStr :: (String -> String -> a) -> JSVal -> JSVal -> Runtime a
liftStr op a b = do
  n1 <- toString a
  n2 <- toString b
  return $ n1 `op` n2


-- ref 11.9.6, incomplete
tripleEquals :: JSVal -> JSVal -> Runtime JSVal
tripleEquals v1 v2 = return $ VBool $ eq v1 v2
  where eq x y
          | typeof x /= typeof y        = False
          | typeof x == TypeUndefined   = True
          | typeof x == TypeNull        = True
          | typeof x == TypeNumber      = x == y
          | typeof x == TypeString      = x == y
          | typeof x == TypeBoolean     = x == y
          | typeof x == TypeObject      = x == y
          | typeof x == TypeFunction    = x == y
          | otherwise = error $ "Can't === " ++ show x ++ " and " ++ show y

doubleEquals :: JSVal -> JSVal -> Runtime JSVal
doubleEquals v1 v2 = return $ VBool $ eq v1 v2
  where eq x y
          | typeof x == typeof y = eq' x y
          | typeof x == TypeUndefined && typeof y == TypeNull = True
          | typeof x == TypeNull && typeof y == TypeUndefined = True
          | otherwise = False
        eq' VUndef VUndef = True
        eq' VNull VNull = True
        eq' (VNum a) (VNum b) = a == b
        eq' (VStr a) (VStr b) = a == b
        eq' (VBool a) (VBool b) = a == b
        eq' (VObj a) (VObj b) = a == b
        eq' _ _ = False

-- ref 15.3.5.3
hasInstance :: Shared JSObj -> JSVal -> Runtime Bool
hasInstance f val = case val of
  VObj obj -> hasInstance' f obj
  _ -> return False

hasInstance' :: Shared JSObj -> Shared JSObj -> Runtime Bool
hasInstance' f val = do
  fun <- objGetProperty "prototype" f
  case fun of
    Nothing -> raiseError "Object has no prototype"
    Just o -> do
      v <- propValue o
      if typeof v /= TypeObject
      then raiseError "TypeError"
      else searchPrototypes v val
  where
    searchPrototypes :: JSVal -> Shared JSObj -> Runtime Bool
    searchPrototypes o v = do
      v' <- objPrototype <$> deref v
      case v' of
        Nothing -> return False
        Just p  -> do
          if o == VObj p
          then return True
          else searchPrototypes o p

-- ref 11.8.6
jsInstanceOf :: JSVal -> JSVal -> Runtime JSVal
jsInstanceOf val cls =
  let typeError = raiseError "TypeError"
  in case cls of
    VObj objRef -> do
      callable <- isJust . callMethod <$> deref objRef
      if callable
      then VBool <$> hasInstance objRef val
      else typeError
    _ -> typeError

-- ref 11.10
bitwise :: (Word32 -> Word32 -> Word32) -> JSVal -> JSVal -> Runtime JSVal
bitwise op a b = do
  n1 <- toNumber a
  n2 <- toNumber b
  return $ VNum $ fromIntegral $ floor n1 `op` floor n2

lshift :: JSVal -> JSVal -> Runtime JSVal
lshift lval rval = do
  lnum <- toInt32 lval
  rnum <- toUInt32 rval
  return $ VNum $ fromIntegral $ shiftL lnum (fromIntegral rnum .&. 0x1f)

rshift :: JSVal -> JSVal -> Runtime JSVal
rshift lval rval = do
  lnum <- toInt32 lval
  rnum <- toUInt32 rval
  return $ VNum $ fromIntegral $ shiftR lnum (fromIntegral rnum .&. 0x1f)

urshift :: JSVal -> JSVal -> Runtime JSVal
urshift lval rval = do
  lnum <- toUInt32 lval
  rnum <- toUInt32 rval
  return $ VNum $ fromIntegral $ shiftR lnum (fromIntegral rnum .&. 0x1f)

chain :: Monad m => (a->m b) -> (b -> m b) -> (b -> m c) -> a -> m c
chain f g h a = f a >>= g >>= h

unaryOp :: (a -> JSVal) -> (JSVal -> Runtime a) -> (a -> a) -> JSVal -> Runtime JSVal
unaryOp toVal fromVal f = chain fromVal (return . f) (return . toVal)

unaryNumOp :: (JSNum->JSNum) -> JSVal -> Runtime JSVal
unaryNumOp = unaryOp VNum toNumber

-- ref 11.4.6
unaryPlus :: JSVal -> Runtime JSVal
unaryPlus = unaryNumOp id

-- ref 11.4.7
unaryMinus :: JSVal -> Runtime JSVal
unaryMinus = unaryNumOp negate

-- ref 11.4.8
unaryBitwiseNot :: JSVal -> Runtime JSVal
unaryBitwiseNot = unaryOp (VNum . fromIntegral) toWord32 (complement :: Word32 -> Word32)
  where toWord32 = toNumber >=> return . floor

-- ref 11.4.9
unaryNot :: JSVal -> Runtime JSVal
unaryNot = unaryOp VBool (return . toBoolean) not

-- ref 11.14
commaOperator :: JSVal -> JSVal -> Runtime JSVal
commaOperator _ = return


mathFunc :: (Double -> Double) -> JSFunction
mathFunc f _this args = liftM (VNum . JSNum . f . fromJSNum) $ toNumber (head args)

mathFunc2 :: (Double -> Double -> Double) -> JSFunction
mathFunc2 f _this args = do
  [a, b] <- mapM toNumber (take 2 args)
  return $ VNum $ JSNum $ f (fromJSNum a) (fromJSNum b)

mathMaxFunc :: ([JSNum] -> JSNum) -> JSFunction
mathMaxFunc f _this args = liftM (VNum . f) $ mapM toNumber args

hypot :: Floating a => a -> a -> a
hypot a b = sqrt (a*a + b*b)

pow :: RealFloat a => a -> a -> a
pow x y
  | abs x == 1 && isInfinite y = 0/0
  | isNegativeZero x && y < 0 && isOddInteger y = -1/0
  | isNegativeZero x && y < 0 && not (isOddInteger y) = 1/0
  | x == 0 && y < 0 = 1/0
  | otherwise = x ** y

isOddInteger :: RealFloat a => a -> Bool
isOddInteger y = not (isInfinite y) && isInteger ((y+1)/2) && abs y < 1e20
