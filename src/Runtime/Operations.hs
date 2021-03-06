{-# Language LambdaCase, OverloadedStrings #-}

module Runtime.Operations where

import Control.Lens hiding (op)
import Control.Monad
import Control.Monad.Trans
import Control.Applicative
import Data.Fixed (mod')
import Data.Word
import Data.Int
import Data.Bits
import Data.Maybe
import Data.Monoid
import qualified Data.Text as T
import Data.Text (Text)
import Expr
import Runtime.Types
import Runtime.Conversion
import Runtime.Object
import Runtime.Shared
import Runtime.PropMap

evalBinOp :: Text -> JSVal -> JSVal -> Runtime JSVal
evalBinOp op = case op of
  "==="        -> {-# SCC binOp3q #-}         tripleEquals
  "!=="        -> {-# SCC binOpN3q #-}        invert tripleEquals
  "=="         -> {-# SCC binOpEq #-}         doubleEquals
  "!="         -> {-# SCC binOpNeq #-}        invert doubleEquals
  "instanceof" -> {-# SCC binOpInstanceof #-} jsInstanceOf
  "in"         -> {-# SCC binOpIn #-}         jsHasProperty
  "+"          -> {-# SCC binOpPlus #-}       jsAdd
  "-"          -> {-# SCC binOpMinus #-}      numberOp (-)
  "*"          -> {-# SCC binOpTimes #-}      numberOp (*)
  "/"          -> {-# SCC binOpDivide #-}     numberOp (/)
  "%"          -> {-# SCC binOpMod #-}        numberOp $ fmod
  "<"          -> {-# SCC binOpLT #-}         lessThan [LT]
  ">"          -> {-# SCC binOpGT #-}         lessThan [GT]
  "<="         -> {-# SCC binOpLE #-}         lessThan [LT, EQ]
  ">="         -> {-# SCC binOpGE #-}         lessThan [GT, EQ]
  "&"          -> {-# SCC binOpAnd #-}        bitwise (.&.)           -- ref 11.10
  "|"          -> {-# SCC binOpOr #-}         bitwise (.|.)           -- ref 11.10
  "^"          -> {-# SCC binOpXor #-}        bitwise xor             -- ref 11.10
  "<<"         -> {-# SCC binOpLshift #-}     lshift
  ">>"         -> {-# SCC binOpRshift #-}     rshift
  ">>>"        -> {-# SCC binOpURshift #-}    urshift
  ","          -> {-# SCC binOpComma #-}      commaOperator
  _            -> noSuchBinop op
  where invert f x y = f x y >>= \case
                                  VUndef      -> return (VBool False)
                                  VBool True  -> return (VBool False)
                                  VBool False -> return (VBool True)
                                  v  -> error $ "Can't invert " ++ show v

noSuchBinop :: Text -> JSVal -> JSVal -> Runtime JSVal
noSuchBinop op a b = raiseError . T.pack $
  "No binop `" ++ (T.unpack op) ++ "' on " ++ show (a, b)



-- ref 11.6.1, incomplete
jsAdd :: JSVal -> JSVal -> Runtime JSVal
jsAdd a b = do
  a' <- toPrimitive HintNone a
  b' <- toPrimitive HintNone b
  if isString a' || isString b'
  then VStr <$> liftStr (<>) a' b'
  else numOp vAdd a' b'

numOp :: (JSVal -> JSVal -> JSVal) -> JSVal -> JSVal -> Runtime JSVal
numOp f a b = do
  a' <- toNum a
  b' <- toNum b
  return (f a' b')


vAdd :: JSVal -> JSVal -> JSVal
vAdd (VInt n) (VInt m) = VInt (n+m)
vAdd (VInt n) (VNum m) = VNum (fromIntegral n + m)
vAdd (VNum n) (VInt m) = VNum (n + fromIntegral m)
vAdd (VNum n) (VNum m) = VNum (n + m)
vAdd a b = error $ unwords [ "Can't add", show a, " and ", show b ]

-- ref 11.8.5 (1)
lessThan :: [Ordering] -> JSVal -> JSVal -> Runtime JSVal
lessThan orderings a b = do
  a' <- toPrimitive HintNumber a
  b' <- toPrimitive HintNumber b
  if isString a' && isString b'
  then cmp =<< cmpStrings a' b'
  else cmpNumbers a' b' orderings

  where cmp :: Maybe Ordering -> Runtime JSVal
        cmp = return . VBool . cmp'
        cmp' (Just o) = o `elem` orderings
        cmp' Nothing = False

-- ref 11.8.5 (3)
cmpNumbers :: JSVal -> JSVal -> [Ordering] -> Runtime JSVal
cmpNumbers x y orderings = do
  a <- toNumber x
  b <- toNumber y
  return (f a b) where
    g ord = if ord `elem` orderings
               then vTrue
               else vFalse
    f nx ny
      | isNaN nx = vFalse
      | isNaN ny = vFalse
      | nx == ny = g EQ
      | nx == (1/0) = g GT
      | ny == (1/0) = g LT
      | ny == (-1/0) = g GT
      | nx == (-1/0) = g LT
      | nx < ny = g LT
      | otherwise = g GT

cmpStrings :: JSVal -> JSVal -> Runtime (Maybe Ordering)
cmpStrings (VStr x) (VStr y) = return $ Just $ compare x y
cmpStrings _ _ = return (Just EQ)

numberOp :: (Double->Double->Double) -> JSVal -> JSVal -> Runtime JSVal
numberOp op a b = do
  a' <- toPrimitive HintNone a
  b' <- toPrimitive HintNone b
  VNum <$> liftNum op a' b'

fmod :: Double -> Double -> Double
fmod n' d' = fmod' (abs n') (abs d') * signum n'
  where fmod' n d
          | isNaN n      = 0/0
          | isNaN n      = 0/0
          | d == 0       = 0/0
          | isInfinite n = 0/0
          | isInfinite d = n
          | otherwise    = n - d * q
            where q = fromIntegral $ truncate (n/d)
          -- | n >= 0       = Double $ mod' n d
          -- | n < 0        = Double $ -mod' (-n) d

fquot :: Double -> Double -> Double
fquot n d = fromIntegral $ floor (n/d)

boolOp :: (JSVal->JSVal->Bool) -> JSVal -> JSVal -> Runtime JSVal
boolOp op a b = return (VBool $ op a b)

liftNum :: (Double -> Double -> a) -> JSVal -> JSVal -> Runtime a
liftNum op a b = do
  n1 <- toNumber a
  n2 <- toNumber b
  return $ n1 `op` n2

liftStr :: (Text -> Text -> a) -> JSVal -> JSVal -> Runtime a
liftStr op a b = do
  n1 <- toString a
  n2 <- toString b
  return $ n1 `op` n2


-- ref 11.9.6, incomplete
tripleEquals :: JSVal -> JSVal -> Runtime JSVal
tripleEquals v1 v2 = return $ VBool $ v1 `eqv` v2

eqv :: JSVal -> JSVal -> Bool
eqv x y
  | typeof x /= typeof y        = False
  | typeof x == TypeUndefined   = True
  | typeof x == TypeNull        = True
  | typeof x == TypeNumber      = eqvNumber x y
  | typeof x == TypeString      = x `sameValue` y
  | typeof x == TypeBoolean     = x `sameValue` y
  | typeof x == TypeObject      = x `sameValue` y
  | typeof x == TypeFunction    = x `sameValue` y
  | otherwise = error $ "Can't === " ++ show x ++ " and " ++ show y

eqvNumber :: JSVal -> JSVal -> Bool
eqvNumber (VNum a) (VNum b) = a == b
eqvNumber (VNum a) (VInt b) = a == fromIntegral b
eqvNumber (VInt a) (VNum b) = fromIntegral a == b
eqvNumber (VInt a) (VInt b) = a == b
eqvNumber _ _ = False


-- ref 11.9.3
doubleEquals :: JSVal -> JSVal -> Runtime JSVal
doubleEquals v1 v2 = eq v1 v2
  where eq x y = if typeof x == typeof y
                 then return (VBool $ eq' x y)
                 else eq'' (typeof x, typeof y) x y
        eq' VUndef VUndef = True
        eq' VNull VNull = True
        eq' (VNum a) (VNum b) = a == b
        eq' (VInt a) (VNum b) = fromIntegral a == b
        eq' (VNum a) (VInt b) = a == fromIntegral b
        eq' (VInt a) (VInt b) = a == b
        eq' (VStr a) (VStr b) = a == b
        eq' (VBool a) (VBool b) = a == b
        eq' (VObj a) (VObj b) = a == b
        eq' _ _ = False

        eq'' :: (JSType, JSType) -> JSVal -> JSVal -> Runtime JSVal
        eq'' (tx, ty) x y = case (tx, ty) of
          (TypeUndefined, TypeNull) -> return $ VBool True
          (TypeNull, TypeUndefined) -> return $ VBool True
          (TypeNumber, TypeString)  -> (x `doubleEquals`) =<< (VNum <$> toNumber y)
          (TypeString, TypeNumber)  -> (`doubleEquals` y) =<< (VNum <$> toNumber x)
          (_, TypeBoolean)          -> (x `doubleEquals`) =<< (VNum <$> toNumber y)
          (TypeBoolean, _)          -> (`doubleEquals` y) =<< (VNum <$> toNumber x)
          (TypeString, TypeObject)  -> (x `doubleEquals`) =<< (toPrimitive HintNone y)
          (TypeNumber, TypeObject)  -> (x `doubleEquals`) =<< (toPrimitive HintNone y)
          (TypeObject, TypeString)  -> (`doubleEquals` y) =<< (toPrimitive HintNone x)
          (TypeObject, TypeNumber)  -> (`doubleEquals` y) =<< (toPrimitive HintNone x)
          _ -> return $ VBool False

-- ref 11.8.6
jsInstanceOf :: JSVal -> JSVal -> Runtime JSVal
jsInstanceOf val cls =
  let typeError = raiseProtoError TypeError "Expecting a function in instanceof"
  in case cls of
    VObj objRef -> do
      view hasInstanceMethod <$> deref objRef >>= \case
        Just method -> VBool <$> method objRef val
        Nothing     -> typeError
    _ -> typeError

-- ref 11.8.7
jsHasProperty :: JSVal -> JSVal -> Runtime JSVal
jsHasProperty lval rval = case rval of
  VObj obj -> do
    name <- toString lval
    VBool <$> objHasProperty name obj
  _ -> raiseProtoError TypeError $
         "Cannot search for " ++ show lval ++ " in " ++ show rval



-- ref 11.10
bitwise :: (Word32 -> Word32 -> Word32) -> JSVal -> JSVal -> Runtime JSVal
bitwise op a b = do
  n1 <- toNumber a
  n2 <- toNumber b
  return $ VInt $ fromIntegral $ floor n1 `op` floor n2

lshift :: JSVal -> JSVal -> Runtime JSVal
lshift lval rval = do
  lnum <- toInt32 lval
  rnum <- toUInt32 rval
  return $ VInt $ fromIntegral $ shiftL lnum (fromIntegral rnum .&. 0x1f)

rshift :: JSVal -> JSVal -> Runtime JSVal
rshift lval rval = do
  lnum <- toInt32 lval
  rnum <- toUInt32 rval
  return $ VInt $ fromIntegral $ shiftR lnum (fromIntegral rnum .&. 0x1f)

urshift :: JSVal -> JSVal -> Runtime JSVal
urshift lval rval = do
  lnum <- toUInt32 lval
  rnum <- toUInt32 rval
  return $ VInt $ fromIntegral $ shiftR lnum (fromIntegral rnum .&. 0x1f)

chain :: Monad m => (a->m b) -> (b -> m b) -> (b -> m c) -> a -> m c
chain f g h a = f a >>= g >>= h

unaryOp :: (a -> JSVal) -> (JSVal -> Runtime a) -> (a -> a) -> JSVal -> Runtime JSVal
unaryOp toVal fromVal f = chain fromVal (return . f) (return . toVal)

unaryNumOp :: (Double->Double) -> JSVal -> Runtime JSVal
unaryNumOp = unaryOp VNum toNumber

-- ref 11.4.6
unaryPlus :: JSVal -> Runtime JSVal
unaryPlus = unaryNumOp id

-- ref 11.4.7
unaryMinus :: JSVal -> Runtime JSVal
unaryMinus = unaryNumOp negate

-- ref 11.4.8
unaryBitwiseNot :: JSVal -> Runtime JSVal
unaryBitwiseNot = unaryOp (VInt . fromIntegral) toInt32 (complement :: Int32 -> Int32)
  -- where toWord32 = toNumber >=> return . floor

-- ref 11.4.9
unaryNot :: JSVal -> Runtime JSVal
unaryNot = unaryOp VBool (return . toBoolean) not

-- ref 11.14
commaOperator :: JSVal -> JSVal -> Runtime JSVal
commaOperator _ = return

isOddInteger :: RealFloat a => a -> Bool
isOddInteger y = not (isInfinite y) && isInteger ((y+1)/2) && abs y < 1e20
