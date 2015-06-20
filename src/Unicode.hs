module Unicode where

import Data.Char
import qualified Data.Set as S


spaceChars :: S.Set Char
spaceChars = S.fromList (lineBreaks ++ iso8859Spaces ++ unicodeSpaces)
  where lineBreaks    = [ '\n', '\r', '\x2028', '\x2029' ]
        iso8859Spaces = [ '\t', '\v', '\f', ' ', '\xa0'  ]
        unicodeSpaces = map chr [ 0x2000..0x200a ] ++
                        [ '\x1680', '\x180e', '\x202f' ] ++
                        [ '\x205f', '\x3000', '\xfeff' ]

isJsSpace :: Char -> Bool
isJsSpace ch = ch `S.member` spaceChars
