module Runtime.Operations where

import Control.Monad
import Control.Applicative
import Data.Word
import Data.Bits
import Expr
import Runtime.Types
import Runtime.Conversion

-- ref 11.6.1, incomplete
jsAdd :: JSVal -> JSVal -> JSRuntime JSVal
jsAdd a b = do
  a' <- toPrimitive HintNone a
  b' <- toPrimitive HintNone b
  if isString a' || isString b'
  then VStr <$> liftStr (++) a' b'
  else VNum <$> liftNum (+) a' b'

-- ref 11.8.5
compareOp :: (Bool -> Bool) -> JSVal -> JSVal -> JSRuntime JSVal
compareOp op a b = do
  a' <- toPrimitive HintNumber a
  b' <- toPrimitive HintNumber b
  liftM (VBool . op) $ if isString a' && isString b'
  then liftStr (<) a' b'
  else liftNum (<) a' b'

numberOp :: (JSNum->JSNum->JSNum) -> JSVal -> JSVal -> JSRuntime JSVal
numberOp op a b = do
  a' <- toPrimitive HintNone a
  b' <- toPrimitive HintNone b
  VNum <$> liftNum op a' b'

boolOp :: (JSVal->JSVal->Bool) -> JSVal -> JSVal -> JSRuntime JSVal
boolOp op a b = return (VBool $ op a b)

liftNum :: (JSNum -> JSNum -> a) -> JSVal -> JSVal -> JSRuntime a
liftNum op a b = do
  n1 <- toNumber a
  n2 <- toNumber b
  return $ n1 `op` n2

liftStr :: (String -> String -> a) -> JSVal -> JSVal -> JSRuntime a
liftStr op a b = do
  n1 <- toString a
  n2 <- toString b
  return $ n1 `op` n2


-- ref 11.9.6, incomplete
tripleEquals :: (Bool->Bool) -> JSVal -> JSVal -> JSRuntime JSVal
tripleEquals op x y = return $ VBool $ op $ eq x y
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

doubleEquals :: (Bool->Bool) -> JSVal -> JSVal -> JSRuntime JSVal
doubleEquals op x y = return $ VBool $ op $ eq x y
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

jsInstanceOf :: JSVal -> JSVal -> JSRuntime JSVal
jsInstanceOf _a _b = return $ VBool True

-- ref 11.10
bitwise :: (Word32 -> Word32 -> Word32) -> JSVal -> JSVal -> JSRuntime JSVal
bitwise op a b = do
  n1 <- toNumber a
  n2 <- toNumber b
  return $ VNum $ fromIntegral $ floor n1 `op` floor n2


chain :: Monad m => (a->m b) -> (b -> m b) -> (b -> m c) -> a -> m c
chain f g h a = f a >>= g >>= h

unaryOp :: (a -> JSVal) -> (JSVal -> JSRuntime a) -> (a -> a) -> JSVal -> JSRuntime JSVal
unaryOp toVal fromVal f = chain fromVal (return . f) (return . toVal)

unaryNumOp :: (JSNum->JSNum) -> JSVal -> JSRuntime JSVal
unaryNumOp = unaryOp VNum toNumber

-- ref 11.4.6
unaryPlus :: JSVal -> JSRuntime JSVal
unaryPlus = unaryNumOp id

-- ref 11.4.7
unaryMinus :: JSVal -> JSRuntime JSVal
unaryMinus = unaryNumOp negate

-- ref 11.4.8
unaryBitwiseNot :: JSVal -> JSRuntime JSVal
unaryBitwiseNot = unaryOp (VNum . fromIntegral) toWord32 (complement :: Word32 -> Word32)
  where toWord32 = toNumber >=> return . floor

-- ref 11.4.9
unaryNot :: JSVal -> JSRuntime JSVal
unaryNot = unaryOp VBool (return . toBoolean) not

-- ref 11.14
commaOperator :: JSVal -> JSVal -> JSRuntime JSVal
commaOperator _ = return
