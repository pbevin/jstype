{-# LANGUAGE LambdaCase #-}

module Parse.State where

import Control.Monad (void)
import Control.Applicative
import Text.Parsec (getState, putState)
import Data.List
import Data.Maybe
import Expr

import Parse.Types


initialParseState :: Strictness -> Bool -> ParseState
initialParseState strict inFunction =
  ParseState { inKeywordAllowed = True,
               insideIteration = False,
               insideFunction = inFunction,
               insideSwitch = False,
               strictnessState = strict,
               labelSet = [],
               contextDescription = Nothing }

removeIn :: [String] -> JSParser [String]
removeIn ops = do
  st <- getState
  return $ if inKeywordAllowed st
           then ops
           else ops \\ ["in"]

withLabel :: Label -> JSParser a -> JSParser a
withLabel label = recurseState addLabel
  where addLabel st = st { labelSet = label : labelSet st }

withStrictness :: Strictness -> JSParser a -> JSParser a
withStrictness isStrict = recurseState $ \st -> st { strictnessState = isStrict }

withoutInKeyword :: JSParser a -> JSParser a
withoutInKeyword = recurseState $ \st -> st { inKeywordAllowed = False }

enterIteration :: JSParser a -> JSParser a
enterIteration = recurseState $ \st -> st { insideIteration = True }

enterSwitch :: JSParser a -> JSParser a
enterSwitch = recurseState $ \st -> st { insideSwitch = True }

enterFunction :: JSParser a -> JSParser a
enterFunction = recurseState $ \st -> st { insideIteration = False, insideSwitch = False }

withFunctionContext :: Maybe String -> JSParser a -> JSParser a
withFunctionContext fname =
  let here = fromMaybe "anonymous function" fname
  in recurseState $ \st -> st { contextDescription = Just here,
                                insideFunction = True }

ifContext :: (ParseState -> Bool) -> String -> JSParser a -> JSParser a
ifContext pred msg p = do
  st <- getState
  if pred st
  then p
  else fail msg


ifInsideFunction :: JSParser a -> JSParser a
ifInsideFunction = ifContext insideFunction $
  "Return statement found outside a function"

ifInsideIterationOrSwitch :: JSParser a -> JSParser a
ifInsideIterationOrSwitch = ifContext insideFunctionOrSwitch $
  "break statement found outside loop or switch statement"
    where
      insideFunctionOrSwitch st = insideFunction st || insideSwitch st

ifInsideIteration :: JSParser a -> JSParser a
ifInsideIteration = ifContext insideIteration $
  "break and continue belong in loops"

fromLabelSet :: JSParser String -> JSParser String
fromLabelSet p = do
  st <- getState
  label <- p
  if label `elem` labelSet st
  then return label
  else fail "Can't break or continue outside current label set"


currentContext :: JSParser (Maybe String)
currentContext = contextDescription <$> getState

ifStrict :: JSParser a -> JSParser ()
ifStrict p = do
  getStrictness >>= \case
    Strict -> void p
    NotStrict -> return ()

getStrictness :: JSParser Strictness
getStrictness = strictnessState <$> getState




recurseState :: (ParseState -> ParseState) -> JSParser a -> JSParser a
recurseState f p = do
  st <- getState
  putState (f st)
  result <- p
  putState st
  return result
