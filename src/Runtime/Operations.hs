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
  else return $ VNum $ addNumbers (toNumber a') (toNumber b')

addNumbers :: JSNum -> JSNum -> JSNum
addNumbers a b = a + b



-- ref 11.8.6, incomplete
jsInstanceOf :: JSVal -> JSVal -> JSRuntime JSVal
jsInstanceOf a b = return $ VBool True
