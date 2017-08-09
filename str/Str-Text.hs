
module Str(
    Str,
    linesCR, S.stripPrefix,
    readFileUTF8,
    S.null, S.isPrefixOf, S.drop, S.span, S.length, toList, S.all, S.uncons,
    ugly, showLength
    ) where

import qualified Data.Text as S
import qualified Data.Text.IO as S

type Str = S.Text

toList = S.unpack

showLength x = show x

linesCR = S.lines

ugly :: Integral a => Integer -> a
ugly = fromInteger

readFileUTF8 = S.readFile
