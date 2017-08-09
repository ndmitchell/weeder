
module Str(
    Str,
    linesCR, S.stripPrefix,
    readFileUTF8,
    S.null, S.isPrefixOf, S.drop, S.span, S.length, toList, S.all, S.uncons,
    showLength,
    ugly
    ) where

import qualified Data.ByteString.Char8 as S

type Str = S.ByteString

toList = S.unpack
showLength x = show x

removeR :: Str -> Str
removeR s | Just (s, c) <- S.unsnoc s, c == '\r' = s
          | otherwise = s

linesCR :: Str -> [Str]
linesCR = map removeR . S.lines

ugly :: Integral a => Integer -> a
ugly = fromInteger

readFileUTF8 = S.readFile
