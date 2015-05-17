module Parse.State where

import Text.Parsec
import Control.Applicative
import Data.List
import Data.Maybe

import Parse.Types



disableInKeyword, enableInKeyword :: JSParser ()
disableInKeyword = modifyState $ \(_, cxt) -> (False, cxt)
enableInKeyword  = modifyState $ \(_, cxt) -> (True, cxt)

withoutInKeyword :: JSParser a -> JSParser a
withoutInKeyword p = disableInKeyword *> p <* enableInKeyword

removeIn :: [String] -> JSParser [String]
removeIn ops = do
  (inKeywordEnabled, _) <- getState
  return $ if inKeywordEnabled
           then ops
           else ops \\ ["in"]

withFunctionContext :: Maybe String -> JSParser a -> JSParser a
withFunctionContext fname p =
  let here = fromMaybe "anonymous function" fname
  in do
    (a, cxt) <- getState
    putState (a, Just here)
    result <- p
    putState (a, cxt)
    return result

currentContext :: JSParser (Maybe String)
currentContext = snd <$> getState

