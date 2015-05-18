module Parse.Types where

import Text.Parsec (Parsec)

data ParseState = ParseState {
  inKeywordAllowed :: Bool,
  insideIteration :: Bool,
  labelSet :: [String],
  contextDescription :: Maybe String }
type JSParser = Parsec String ParseState
