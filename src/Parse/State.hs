{-# LANGUAGE LambdaCase, OverloadedStrings #-}

module Parse.State where

import Control.Monad (void)
import Control.Applicative
import Text.Parsec (getState, putState, runP, ParseError, SourceName)
import qualified Data.Text as T
import Data.Text (Text)
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
               insideDirectivePrologue = False,
               strictnessState = strict,
               labelSet = [],
               immediateLabelSet = [],
               contextDescription = Nothing }

removeIn :: [Text] -> JSParser [Text]
removeIn ops = do
  st <- getState
  return $ if inKeywordAllowed st
           then ops
           else ops \\ ["in"]

withLabel :: Label -> JSParser a -> JSParser a
withLabel label = recurseState addLabel
  where addLabel st = st { labelSet = label : labelSet st, immediateLabelSet = label : immediateLabelSet st }

withStrictness :: Strictness -> JSParser a -> JSParser a
withStrictness isStrict = recurseState $ \st -> st { strictnessState = isStrict }

withDirectivePrologue :: JSParser a -> JSParser a
withDirectivePrologue = recurseState $ \st -> st { insideDirectivePrologue = True }

withoutInKeyword :: JSParser a -> JSParser a
withoutInKeyword = recurseState $ \st -> st { inKeywordAllowed = False }

enterIteration :: JSParser a -> JSParser a
enterIteration = recurseState $ \st -> st { insideIteration = True }

enterSwitch :: JSParser a -> JSParser a
enterSwitch = recurseState $ \st -> st { insideSwitch = True }

enterFunction :: JSParser a -> JSParser a
enterFunction = recurseState $ \st -> st { insideIteration = False, insideSwitch = False }

withFunctionContext :: Maybe Text -> JSParser a -> JSParser a
withFunctionContext fname =
  let here = fromMaybe "anonymous function" fname
  in recurseState $ \st -> st { contextDescription = Just here,
                                insideFunction = True }

ifContext :: (ParseState -> Bool) -> JSParser a -> JSParser a -> JSParser a
ifContext pred ifNo ifYes = do
  st <- getState
  if pred st then ifYes else ifNo

ifInsideFunction :: JSParser a -> JSParser a
ifInsideFunction = ifContext insideFunction $
  fail "Return statement found outside a function"

ifInsideIterationOrSwitch :: JSParser a -> JSParser a
ifInsideIterationOrSwitch = ifContext insideFunctionOrSwitch $
  fail "break statement found outside loop or switch statement"
    where
      insideFunctionOrSwitch st = insideIteration st || insideSwitch st

ifInsideIteration :: JSParser a -> JSParser a
ifInsideIteration = ifContext insideIteration $
  fail "break and continue belong in loops"

ifInDirectivePrologue :: JSParser a -> JSParser a -> JSParser a
ifInDirectivePrologue = flip (ifContext insideDirectivePrologue)


fromLabelSet :: JSParser Text -> JSParser Text
fromLabelSet p = do
  st <- getState
  label <- p
  if label `elem` labelSet st
  then return label
  else fail "Can't break or continue outside current label set"

clearCurrentLabelSet :: JSParser a -> JSParser a
clearCurrentLabelSet = recurseState $ \st -> st { immediateLabelSet = [] }



currentLabelSet :: JSParser [Text]
currentLabelSet = immediateLabelSet <$> getState

currentContext :: JSParser (Maybe Text)
currentContext = contextDescription <$> getState

ifStrict :: JSParser a -> JSParser ()
ifStrict p = do
  getStrictness >>= \case
    Strict -> void p
    NotStrict -> return ()

ifNotStrict :: JSParser a -> JSParser a
ifNotStrict p = do
  getStrictness >>= \case
    Strict    -> fail "Not allowed in strict mode"
    NotStrict -> p

failIfStrict :: JSParser ()
failIfStrict = ifStrict (fail "")

getStrictness :: JSParser Strictness
getStrictness = strictnessState <$> getState



jsParse :: JSParser a -> Strictness -> Bool -> SourceName -> Text -> Either ParseError a
jsParse p strict inFunction name str = runP p (initialParseState strict inFunction) name str

runp :: JSParser a -> Text -> Either ParseError a
runp p input = runP p (initialParseState NotStrict False) "" input

recurseState :: (ParseState -> ParseState) -> JSParser a -> JSParser a
recurseState f p = do
  st <- getState
  putState (f st)
  result <- p
  putState st
  return result
