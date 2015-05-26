module Parse.State where

import Text.Parsec (getState, putState)
import Control.Applicative
import Data.List
import Data.Maybe
import Expr

import Parse.Types


initialParseState :: Strictness -> Bool -> ParseState
initialParseState strict inFunction =
  ParseState { inKeywordAllowed = True,
               insideIteration = False,
               insideFunction = inFunction,
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

withInsideIteration :: JSParser a -> JSParser a
withInsideIteration = recurseState $ \st -> st { insideIteration = True }

withoutInsideIteration :: JSParser a -> JSParser a
withoutInsideIteration = recurseState $ \st -> st { insideIteration = False }

withFunctionContext :: Maybe String -> JSParser a -> JSParser a
withFunctionContext fname =
  let here = fromMaybe "anonymous function" fname
  in recurseState $ \st -> st { contextDescription = Just here,
                                insideFunction = True }

ifInsideFunction :: JSParser a -> JSParser a
ifInsideFunction p = do
  st <- getState
  if insideFunction st
  then p
  else fail "Return statement found outside a function"

ifInsideIteration :: JSParser a -> JSParser a
ifInsideIteration p = do
  st <- getState
  if insideIteration st
  then p
  else fail "break and continue belong in loops"

fromLabelSet :: JSParser String -> JSParser String
fromLabelSet p = do
  st <- getState
  label <- p
  if label `elem` labelSet st
  then return label
  else fail "Can't break or continue outside current label set"


currentContext :: JSParser (Maybe String)
currentContext = contextDescription <$> getState

getStrictness :: JSParser Strictness
getStrictness = strictnessState <$> getState




recurseState :: (ParseState -> ParseState) -> JSParser a -> JSParser a
recurseState f p = do
  st <- getState
  putState (f st)
  result <- p
  putState st
  return result
