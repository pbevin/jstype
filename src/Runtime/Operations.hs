module Runtime.Operations where

import Expr
import Runtime.Types
import Runtime.Conversion

-- ref 11.6.1, incomplete
jsAdd :: JSVal -> JSVal -> JSRuntime JSVal
jsAdd a b = do
  a' <- toPrimitive HintNone a
  b' <- toPrimitive HintNone b
  if isString a' || isString b'
  then return $ VStr $ toString a' ++ toString b'
  else return $ VNum $ toNumber a' + toNumber b'

-- ref 11.8.5
compareOp :: (Bool -> Bool) -> JSVal -> JSVal -> JSRuntime JSVal
compareOp op a b = do
  a' <- toPrimitive HintNumber a
  b' <- toPrimitive HintNumber b
  return $ VBool $ op $ if isString a' && isString b'
  then toString a' < toString b'
  else toNumber a' < toNumber b'

numberOp :: (JSNum->JSNum->JSNum) -> JSVal -> JSVal -> JSRuntime JSVal
numberOp op a b = do
  a' <- toPrimitive HintNone a
  b' <- toPrimitive HintNone b
  return $ VNum $ toNumber a' `op` toNumber b'

boolOp :: (JSVal->JSVal->Bool) -> JSVal -> JSVal -> JSRuntime JSVal
boolOp op a b = return (VBool $ op a b)



-- ref 11.9.6, incomplete
tripleEquals :: JSVal -> JSVal -> Bool
tripleEquals x y
  | typeof x /= typeof y        = False
  | typeof x == TypeUndefined   = True
  | typeof x == TypeNull        = True
  | typeof x == TypeNumber      = x == y
  | typeof x == TypeString      = x == y
  | typeof x == TypeBoolean     = x == y
  | typeof x == TypeObject      = x == y
  | typeof x == TypeFunction    = x == y
  | otherwise = error $ "Can't === " ++ show x ++ " and " ++ show y



--     if isString a' && isString b'
--     then toString a' `op` toString b'
--     else toNumber a' `op` toNumber b'




-- ref 11.8.6, incomplete
jsInstanceOf :: JSVal -> JSVal -> JSRuntime JSVal
jsInstanceOf _a _b = return $ VBool True
