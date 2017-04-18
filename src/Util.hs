
module Util(
    PackageName, ModuleName,
    parseHanging,
    (?:),
    isHaskellSymbol
    ) where

import Data.Char
import Data.Monoid
import Data.List.Extra
import Data.Tuple.Extra
import Prelude


type PackageName = String
type ModuleName = String


-- | Return the first non-empty argument in a left-to-right manner
(?:) :: (Eq a, Monoid a) => a -> a -> a
a ?: b = if a == mempty then b else a

-- | Parse a hanging lines of lines.
parseHanging :: [String] -> [(String, [String])]
parseHanging = repeatedly (\(x:xs) -> first (\a -> (x, unindent a)) $ span (\x -> null x || " " `isPrefixOf` x) xs)

unindent :: [String] -> [String]
unindent xs = map (drop n) xs
    where
        n = minimum $ top : map f xs
        f x = let (a,b) = span isSpace x in if null b then top else length a
        top = 1000

-- | Is the character a member of possible Haskell symbol characters,
--   according to the Haskell report.
isHaskellSymbol :: Char -> Bool
isHaskellSymbol x =
    x `elem` "!#$%&*+./<=>?@\\^|-~" ||
    (isSymbol x && x `notElem` "\"'_(),;[]`{}")
