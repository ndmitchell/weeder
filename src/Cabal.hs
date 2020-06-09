{-# LANGUAGE ViewPatterns, RecordWildCards #-}

module Cabal(
    Cabal(..), CabalSection(..), CabalSectionType,
    parseCabal,
    selectCabalFile,
    selectHiFiles
    ) where

import System.IO.Extra
import System.Directory.Extra
import System.FilePath
import qualified Data.HashMap.Strict as Map
import Util
import Data.Char
import Data.Maybe
import Data.List.Extra
import Data.Tuple.Extra
import Data.Either.Extra
import Data.Semigroup
import Prelude


selectCabalFile :: FilePath -> IO FilePath
selectCabalFile dir = do
    xs <- listFiles dir
    case filter ((==) ".cabal" . takeExtension) xs of
        [x] -> return x
        _ -> fail $ "Didn't find exactly 1 cabal file in " ++ dir

-- | Return the (exposed Hi files, internal Hi files, not found)
selectHiFiles :: FilePath -> Map.HashMap FilePathEq a -> CabalSection -> ([a], [a], [ModuleName])
selectHiFiles distDir his sect@CabalSection{..} = (external, internal, bad1++bad2)
    where
        (bad1, external) = partitionEithers $
            [findHi his sect $ Left cabalMainIs | cabalMainIs /= ""] ++
            [findHi his sect $ Right x | x <- cabalExposedModules]
        (bad2, internal) = partitionEithers
            [findHi his sect $ Right x | x <- filter (not . isPathsModule) cabalOtherModules]

        findHi :: Map.HashMap FilePathEq a -> CabalSection -> Either FilePath ModuleName -> Either ModuleName a
        findHi his cabal@CabalSection{..} name =
            -- error $ show (poss, Map.keys his)
            maybe (Left mname) Right $ firstJust (`Map.lookup` his) poss
            where
                mname = either takeFileName id name
                poss = map filePathEq $ possibleHi distDir cabalSourceDirs cabalSectionType $ either (return . dropExtension) (splitOn ".") name


-- | This code is fragile and keeps going wrong, should probably try a less "guess everything"
--   and a more refined filter and test.
possibleHi :: FilePath -> [FilePath] -> CabalSectionType -> [String] -> [FilePath]
possibleHi distDir sourceDirs sectionType components =
    [ joinPath (root : x : components) <.> "dump-hi"
    | extra <- [".",distDir]
    , root <- concat [["build" </> extra </> x </> (x ++ "-tmp")
                      ,"build" </> extra </> x </> x
                      ,"build" </> extra </> x </> (x ++ "-tmp") </> distDir </> "build" </> x </> (x ++ "-tmp")]
                     | Just x <- [cabalSectionTypeName sectionType]] ++
              ["build", "build" </> distDir </> "build"]
    , x <- sourceDirs ++ ["."]]


data Cabal = Cabal
    {cabalName :: PackageName
    ,cabalSections :: [CabalSection]
    } deriving Show

instance Semigroup Cabal where
    Cabal x1 x2 <> Cabal y1 y2 = Cabal (x1?:y1) (x2++y2)

instance Monoid Cabal where
    mempty = Cabal "" []
    mappend = (<>)

data CabalSectionType = Library (Maybe String) | Executable String | TestSuite String | Benchmark String
    deriving (Eq,Ord)

cabalSectionTypeName :: CabalSectionType -> Maybe String
cabalSectionTypeName (Library x) = x
cabalSectionTypeName (Executable x) = Just x
cabalSectionTypeName (TestSuite x) = Just x
cabalSectionTypeName (Benchmark x) = Just x

instance Show CabalSectionType where
    show (Library Nothing) = "library"
    show (Library (Just x)) = "library:" ++ x
    show (Executable x) = "exe:" ++ x
    show (TestSuite x) = "test:" ++ x
    show (Benchmark x) = "bench:" ++ x

instance Read CabalSectionType where
    readsPrec _ "library" = [(Library Nothing,"")]
    readsPrec _ x
        | Just x <- stripPrefix "exe:" x = [(Executable x, "")]
        | Just x <- stripPrefix "test:" x = [(TestSuite x, "")]
        | Just x <- stripPrefix "bench:" x = [(Benchmark x, "")]
        | Just x <- stripPrefix "library:" x = [(Library (Just x), "")]
    readsPrec _ _ = []

data CabalSection = CabalSection
    {cabalSectionType :: CabalSectionType
    ,cabalMainIs :: FilePath
    ,cabalExposedModules :: [ModuleName]
    ,cabalOtherModules :: [ModuleName]
    ,cabalSourceDirs :: [FilePath]
    ,cabalPackages :: [PackageName]
    } deriving Show

instance Semigroup CabalSection where
    CabalSection x1 x2 x3 x4 x5 x6 <> CabalSection y1 y2 y3 y4 y5 y6 =
        CabalSection x1 (x2?:y2) (x3<>y3) (x4<>y4) (x5<>y5) (x6<>y6)

instance Monoid CabalSection where
    mempty = CabalSection (Library Nothing) "" [] [] [] []
    mappend = (<>)

parseCabal :: FilePath -> IO Cabal
parseCabal = fmap parseTop . readFile'

parseTop = mconcatMap f . parseHanging . filter (not . isComment) . lines
    where
        isComment = isPrefixOf "--" . trimStart
        keyName = (lower *** fst . word1) . word1

        f (keyName -> (key, name), xs) = case key of
            "name:" -> mempty{cabalName=name}
            "library" -> case name of
                "" -> mempty{cabalSections=[parseSection (Library Nothing) xs]}
                x -> mempty{cabalSections=[parseSection (Library (Just x)) xs]}
            "executable" -> mempty{cabalSections=[parseSection (Executable name) xs]}
            "test-suite" -> mempty{cabalSections=[parseSection (TestSuite name) xs]}
            "benchmark" -> mempty{cabalSections=[parseSection (Benchmark name) xs]}
            _ -> mempty

parseSection typ xs = mempty{cabalSectionType=typ} <> parse xs
    where
        parse = mconcatMap f . parseHanging
        keyValues (x,xs) = let (x1,x2) = word1 x in (lower x1, trimEqual $ filter (not . null) $ x2:xs)
        trimEqual xs = map (drop n) xs
            where n = minimum $ 0 : map (length . takeWhile isSpace) xs
        listSplit = concatMap (wordsBy (`elem` " ,"))
        isPackageNameChar x = isAlphaNum x || x == '-'
        parsePackage = dropSuffix "-any" . takeWhile isPackageNameChar . trim

        f (keyValues -> (k,vs)) = case k of
            "if" -> parse vs
            "else" -> parse vs
            "build-depends:" -> mempty{cabalPackages = map parsePackage . splitOn "," $ unwords vs}
            "hs-source-dirs:" -> mempty{cabalSourceDirs=listSplit vs}
            "exposed-modules:" -> mempty{cabalExposedModules=listSplit vs}
            "other-modules:" -> mempty{cabalOtherModules=listSplit vs}
            "main-is:" -> mempty{cabalMainIs=headDef "" vs}
            _ -> mempty
