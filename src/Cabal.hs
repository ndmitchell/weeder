{-# LANGUAGE ViewPatterns #-}

module Cabal(
    Cabal(..), CabalSection(..), CabalSectionType(..),
    parseCabal
    ) where

import System.IO.Extra
import Util
import Data.List.Extra
import Data.Tuple.Extra
import Data.Monoid


data Cabal = Cabal
    {cabalName :: String
    ,cabalSections :: [CabalSection]
    } deriving Show

a <?> b = if a == mempty then b else a

instance Monoid Cabal where
    mempty = Cabal "" []
    mappend (Cabal x1 x2) (Cabal y1 y2) = Cabal (x1 <?> y1) (x2++y2)

data CabalSectionType = Library | Executable | TestSuite deriving Show

data CabalSection = CabalSection
    {cabalSectionType :: CabalSectionType
    ,cabalSectionName :: String
    ,cabalMainIs :: String
    ,cabalExposedModules :: [String]
    ,cabalOtherModules :: [String]
    ,cabalSourceDirs :: [String]
    ,cabalPackages :: [String]
    } deriving Show

instance Monoid CabalSection where
    mempty = CabalSection Library "" "" [] [] [] []
    mappend (CabalSection x1 x2 x3 x4 x5 x6 x7) (CabalSection y1 y2 y3 y4 y5 y6 y7) =
        CabalSection x1 (x2<?>y2) (x3<?>y3) (x4<>y4) (x5<>y5) (x6<>y6) (x7<>y7)


parseCabal :: FilePath -> IO Cabal
parseCabal = fmap parseTop . readFile'

parseTop = foldMap f . parseHanging . filter (not . isComment) . lines
    where
        isComment = isPrefixOf "--" . trimStart
        keyName = (lower *** fst . word1) . word1

        f (keyName -> (key, name), xs) = case key of
            "name:" -> mempty{cabalName=name}
            "library" -> mempty{cabalSections=[parseSection Library "" xs]}
            "executable" -> mempty{cabalSections=[parseSection Executable name xs]}
            "test-suite" -> mempty{cabalSections=[parseSection TestSuite name xs]}
            _ -> mempty

parseSection typ name xs =
    mempty{cabalSectionType=typ, cabalSectionName=name} <>
    foldMap f (parseHanging xs)
    where
        keyValues (x,xs) = let (x1,x2) = word1 x in (lower x1, filter (not . null) $ map trim $ x2:xs)

        f (keyValues -> (k,vs)) = case k of
            "build-depends:" -> mempty{cabalPackages = map (trim . takeWhile (`notElem` "=><")) . splitOn "," $ unwords vs}
            "hs-source-dirs:" -> mempty{cabalSourceDirs=vs}
            "exposed-modules:" -> mempty{cabalExposedModules=vs}
            "other-modules:" -> mempty{cabalOtherModules=vs}
            _ -> mempty
