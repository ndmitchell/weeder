{-# LANGUAGE GeneralizedNewtypeDeriving, OverloadedStrings #-}

module Util(
    Str,
    FilePathEq, filePathEq,
    PackageName, ModuleName, IdentName,
    parseHanging,
    parseHanging2, unindent2,
    (?:),
    isHaskellCtor,
    isHaskellSymbol,
    reachable,
    isPathsModule,
    cmd, cmdStdout
    ) where

import Data.Char
import Data.Monoid
import Data.Hashable
import Data.List.Extra
import Data.Tuple.Extra
import System.Process
import System.FilePath
import System.Directory
import System.Info.Extra
import System.Console.CmdArgs.Verbosity
import Str(Str)
import qualified Str as S
import qualified Data.HashSet as Set
import Prelude


type PackageName = String
type ModuleName = String
type IdentName = String


-- | Return the first non-empty argument in a left-to-right manner
(?:) :: (Eq a, Monoid a) => a -> a -> a
a ?: b = if a == mempty then b else a

-- | Parse a hanging lines of lines.
parseHanging :: [String] -> [(String, [String])]
parseHanging = repeatedly (\(x:xs) -> first (\a -> (x, unindent a)) $ span (maybe True ((== ' ') . fst) . uncons) xs)

parseHanging2 :: [Str] -> [(Str, [Str])]
parseHanging2 = repeatedly (\(x:xs) -> first (\a -> (x, a)) $ span (maybe True ((== ' ') . fst) . S.uncons) xs)

unindent :: [String] -> [String]
unindent xs = map (drop n) xs
    where
        n = minimum $ top : map f xs
        f x = let (a,b) = span isSpace x in if null b then top else length a
        top = 1000

unindent2 :: [Str] -> [Str]
unindent2 xs = map (S.drop n) xs
    where
        n = minimum $ top : map f xs
        f x = let (a,b) = S.span isSpace x in if S.null b then top else S.length a
        top = S.ugly 1000

-- | Is the character a member of possible Haskell symbol characters,
--   according to the Haskell report.
isHaskellSymbol :: Char -> Bool
isHaskellSymbol x =
    x `elem` ("!#$%&*+./<=>?@\\^|-~" :: String) ||
    (isSymbol x && x `notElem` ("\"'_(),;[]`{}" :: String))

isHaskellCtor :: IdentName -> Bool
isHaskellCtor [] = False
isHaskellCtor (x:xs) = isUpper x || x == ':'

-- | Normal 'FilePath' has 'Eq' but it allows non-normalised paths
--   and on Windows is case-sensitive even when the underlying file system isn't.
newtype FilePathEq = FilePathEq FilePath
    deriving (Hashable,Eq,Ord,Show)

filePathEq :: FilePath -> FilePathEq
filePathEq = FilePathEq . (if isWindows then lower else id) . normalise

-- | Given a list of mappings, and an initial set, find which items can be reached
reachable :: (Eq k, Hashable k) => (k -> [k]) -> [k] -> Set.HashSet k
reachable follow = f Set.empty
    where
        f done [] = done
        f done (x:xs)
            | x `Set.member` done = f done xs
            | otherwise = f (Set.insert x done) $ follow x ++ xs

-- | Is a given module name the specially generated cabal Paths_foo module
isPathsModule :: ModuleName -> Bool
isPathsModule = isPrefixOf "Paths_"


cmdTrace :: FilePath -> [String] -> IO ()
cmdTrace exe args = whenLoud $ do
    dir <- getCurrentDirectory
    putStrLn $ "Running: " ++ showCommandForUser exe args ++ " (in " ++ dir ++ ")"

cmd :: FilePath -> [String] -> IO ()
cmd exe args = do
    cmdTrace exe args
    callProcess exe args

cmdStdout :: FilePath -> [String] -> IO String
cmdStdout exe args = do
    cmdTrace exe args
    readCreateProcess (proc exe args) ""
