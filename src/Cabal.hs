{-# LANGUAGE ViewPatterns, RecordWildCards #-}

module Cabal(
    Cabal(..), CabalSection(..), CabalSectionType,
    parseCabal,
    cabalSectionTypeName,
    selectCabalFile
    ) where

import System.IO.Extra
import System.Directory.Extra
import System.FilePath
import Util
import Data.List.Extra
import Data.Tuple.Extra
import Data.Monoid
import Prelude


selectCabalFile :: FilePath -> IO FilePath
selectCabalFile dir = do
    xs <- listFiles dir
    case filter ((==) ".cabal" . takeExtension) xs of
        [x] -> return x
        _ -> fail $ "Didn't find exactly 1 cabal file in " ++ dir

data Cabal = Cabal
    {cabalName :: PackageName
    ,cabalSections :: [CabalSection]
    } deriving Show

instance Monoid Cabal where
    mempty = Cabal "" []
    mappend (Cabal x1 x2) (Cabal y1 y2) = Cabal (x1?:y1) (x2++y2)

data CabalSectionType = Library | Executable String | TestSuite String
    deriving (Eq,Ord)

cabalSectionTypeName :: CabalSectionType -> Maybe String
cabalSectionTypeName Library = Nothing
cabalSectionTypeName (Executable x) = Just x
cabalSectionTypeName (TestSuite x) = Just x

instance Show CabalSectionType where
    show Library = "library"
    show (Executable x) = "executable " ++ x
    show (TestSuite x) = "test-suite " ++ x

data CabalSection = CabalSection
    {cabalSectionType :: CabalSectionType
    ,cabalMainIs :: FilePath
    ,cabalExposedModules :: [ModuleName]
    ,cabalOtherModules :: [ModuleName]
    ,cabalSourceDirs :: [FilePath]
    ,cabalPackages :: [PackageName]
    } deriving Show

instance Monoid CabalSection where
    mempty = CabalSection Library "" [] [] [] []
    mappend (CabalSection x1 x2 x3 x4 x5 x6) (CabalSection y1 y2 y3 y4 y5 y6) =
        CabalSection x1 (x2?:y2) (x3<>y3) (x4<>y4) (x5<>y5) (x6<>y6)


parseCabal :: FilePath -> IO Cabal
parseCabal = fmap parseTop . readFile'

parseTop = mconcat . map f . parseHanging . filter (not . isComment) . lines
    where
        isComment = isPrefixOf "--" . trimStart
        keyName = (lower *** fst . word1) . word1

        f (keyName -> (key, name), xs) = case key of
            "name:" -> mempty{cabalName=name}
            "library" -> mempty{cabalSections=[parseSection Library xs]}
            "executable" -> mempty{cabalSections=[parseSection (Executable name) xs]}
            "test-suite" -> mempty{cabalSections=[parseSection (TestSuite name) xs]}
            _ -> mempty

parseSection typ xs =
    mempty{cabalSectionType=typ} <>
    mconcat (map f $ parseHanging xs)
    where
        keyValues (x,xs) = let (x1,x2) = word1 x in (lower x1, filter (not . null) $ map trim $ x2:xs)
        listModules = concatMap (wordsBy (`elem` " ,"))

        f (keyValues -> (k,vs)) = case k of
            "build-depends:" -> mempty{cabalPackages = map (trim . takeWhile (`notElem` "=><")) . splitOn "," $ unwords vs}
            "hs-source-dirs:" -> mempty{cabalSourceDirs=vs}
            "exposed-modules:" -> mempty{cabalExposedModules=listModules vs}
            "other-modules:" -> mempty{cabalOtherModules=listModules vs}
            "main-is:" -> mempty{cabalMainIs=head $ vs ++ [""]}
            _ -> mempty
