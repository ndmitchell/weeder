{-# LANGUAGE ViewPatterns #-}

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
import qualified Foundation.Conduit as C
import qualified Foundation.Conduit.Textual as C
import Data.Tuple.Extra


type Str = S.String

showLength :: S.CountOf a -> String
showLength (S.CountOf x) = show x

removeR :: Str -> Str
removeR s | Just (s, c) <- S.unsnoc s, c == '\r' = s
          | otherwise = s

linesCR :: Str -> [Str]
linesCR s = map removeR $ C.runConduitPure (C.yield s C..| C.lines C..| C.sinkList)

ugly :: S.Integral a => Integer -> a
ugly = S.fromInteger


readFileUTF8 :: FilePath -> IO Str
readFileUTF8 = fmap (fst3 . S.fromBytes S.UTF8) . S.readFile . S.fromString
