module Parse.Types where

import Data.Text
import Text.Parsec (Parsec)
import Expr (Strictness)

data ParseState = ParseState {
  inKeywordAllowed :: Bool,
  insideIteration :: Bool,
  insideFunction :: Bool,
  insideSwitch :: Bool,
  insideDirectivePrologue :: Bool,
  strictnessState :: Strictness,
  labelSet :: [Text],
  immediateLabelSet :: [Text],
  contextDescription :: Maybe Text } deriving Show
type JSParser = Parsec Text ParseState
