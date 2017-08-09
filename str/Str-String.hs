
module Str(
    Str,
    linesCR, stripPrefix,
    readFileUTF8,
    S.null, S.isPrefixOf, S.drop, S.span, S.length, toList, S.all, S.uncons,
    ugly, showLength
    ) where

import Data.List.Extra as S
import System.IO.Extra

type Str = String

toList = id
showLength x = show x

linesCR = lines

ugly :: Integral a => Integer -> a
ugly = fromInteger
