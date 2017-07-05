
module Str(
    Str,
    linesCR, S.stripPrefix,
    readFileUTF8,
    S.null, S.isPrefixOf, S.drop, S.span, S.length, S.toList, S.all, S.uncons,
    ugly
    ) where

import qualified Foundation as S
import qualified Foundation.String as S
import qualified Foundation.IO as S
import Data.Tuple.Extra


type Str = S.String

linesCR :: Str -> [Str]
linesCR = S.lines

ugly :: S.Integral a => Integer -> a
ugly = S.fromInteger

readFileUTF8 :: FilePath -> IO Str
readFileUTF8 = fmap S.fromBytesUnsafe . S.readFile . S.fromString
