module Parse.Types where

import Text.Parsec (Parsec)
import Expr (Strictness)

data ParseState = ParseState {
  inKeywordAllowed :: Bool,
  insideIteration :: Bool,
  strictness :: Strictness,
  labelSet :: [String],
  contextDescription :: Maybe String }
type JSParser = Parsec String ParseState
