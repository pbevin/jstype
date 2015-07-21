module Parse.Types where

import Text.Parsec (Parsec)
import Expr (Strictness)

data ParseState = ParseState {
  inKeywordAllowed :: Bool,
  insideIteration :: Bool,
  insideFunction :: Bool,
  insideSwitch :: Bool,
  insideDirectivePrologue :: Bool,
  strictnessState :: Strictness,
  labelSet :: [String],
  immediateLabelSet :: [String],
  contextDescription :: Maybe String } deriving Show
type JSParser = Parsec String ParseState
