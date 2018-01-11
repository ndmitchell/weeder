
module Str(
    Str,
    linesCR,
    readFileUTF8,
    S.null, S.isPrefixOf, S.drop, S.span, S.length, S.toList, S.all, S.uncons, S.stripPrefix,
    ugly, showLength
    ) where

import qualified Foundation as S
import qualified Foundation.String as S
import qualified Foundation.IO as S
import Data.Tuple.Extra


type Str = S.String

showLength :: S.CountOf a -> String
showLength (S.CountOf x) = show x

linesCR :: Str -> [Str]
linesCR = S.lines

ugly :: S.Integral a => Integer -> a
ugly = S.fromInteger

readFileUTF8 :: FilePath -> IO Str
readFileUTF8 = fmap (fst3 . S.fromBytes S.UTF8) . S.readFile . S.fromString
