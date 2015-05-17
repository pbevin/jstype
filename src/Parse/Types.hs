module Parse.Types where

import Text.Parsec (Parsec)

type ParseState = (Bool, Maybe String) -- "in" allowed, context
type JSParser = Parsec String ParseState
