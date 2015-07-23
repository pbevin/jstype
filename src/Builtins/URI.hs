{-# LANGUAGE MultiWayIf #-}

module Builtins.URI where

import Control.Applicative
import Data.Char
import qualified Data.Text as T
import Runtime

decodeURI, encodeURI, decodeURIComponent, encodeURIComponent :: String -> Runtime String

-- ref 15.1.3.1
decodeURI str = case decode str of
  Nothing -> raiseURIError $ T.pack str
  Just s  -> return s


data EChar = Chr Char
           | Hex String Int
           | Unc String

decode :: String -> Maybe String
decode str = decodeUTF8 =<< toEChar str

toEChar :: String -> Maybe [EChar]
toEChar str =
  case str of
    []             -> Just []
    ['%']          -> Nothing
    ['%', _]       -> Nothing
    ('%':a:b:rest) -> liftA2 (:) (decode1 a b ";/?:@&=+$,#") (toEChar rest)
    (ch:rest)      -> (Chr ch :) <$> toEChar rest


decode1 :: Char -> Char -> String -> Maybe EChar
decode1 a b uriReserved
  | notHex       = Nothing
  | reservedChar = Just (Unc ['%',a,b])
  | otherwise    = Just (Hex [a,b] val)
  where
    notHex = not (isHexDigit a && isHexDigit b)
    reservedChar = ch `elem` uriReserved
    val = 16 * digitToInt a + digitToInt b
    ch = chr val



decodeUTF8 :: [EChar] -> Maybe String
decodeUTF8 xs = case xs of
  [] -> return ""
  (Chr ch) : rest -> (ch :) <$> decodeUTF8 rest
  (Unc s) : rest  -> (s ++) <$> decodeUTF8 rest
  _ -> do (ch, rest) <- deUtf8 xs
          (chr ch :) <$> decodeUTF8 rest


deUtf8 :: [EChar] -> Maybe (Int, [EChar])
deUtf8 s = bytes ["0xxxxxxx"] <|>
           bytes ["110xxxxx", "10xxxxxx"] <|>
           bytes ["1110xxxx", "10xxxxxx", "10xxxxxx"] <|>
           bytes ["11110xxx", "10xxxxxx", "10xxxxxx", "10xxxxxx"]
  where bytes p = match p (splitAt (length p) s)

match :: [String] -> ([EChar], [EChar]) -> Maybe (Int, [EChar])
match p (as, bs) = decodePattern (zip (concat p) (concatMap (binary 8) as)) >>= \v -> Just (v, bs)

decodePattern :: [(Char, Bool)] -> Maybe Int
decodePattern cs = go 0 cs
  where
    go n [] = Just n
    go n (x:xs) = case x of
      ('0', False) -> go n xs
      ('1', True)  -> go n xs
      ('x', b)     -> go (2*n + k) xs where k = if b then 1 else 0
      _            -> Nothing

binary :: Int -> EChar -> [Bool]
binary k (Hex _ n) = reverse . take k $ (bits n) ++ repeat False
  where bits :: Int -> [Bool]
        bits 0 = []
        bits m = odd m : bits (m `div` 2)
binary _ (Chr ch) = error $ "char " ++ show [ch]


-- ref 15.1.3.2
decodeURIComponent = undefined
-- ref 15.1.3.3
encodeURI = undefined
-- ref 15.1.3.4
encodeURIComponent = undefined
