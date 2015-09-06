module Builtins.ParseFloat (parseFloat) where

import Parse.Number
import Data.Text (Text)

-- ref 15.1.2.3
parseFloat :: Text -> Double
parseFloat = parseDecimal
