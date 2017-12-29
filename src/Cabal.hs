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
        findHi his cabal@CabalSection{..} name = maybe (Left mname) Right $ firstJust (`Map.lookup` his) poss
            where
                mname = either takeFileName id name
                poss = [ filePathEq $ joinPath (root : x : either (return . dropExtension) (splitOn ".") name) <.> "dump-hi"
                    | extra <- [".",distDir]
                    , root <- ["build" </> extra </> x </> (x ++ "-tmp") | Just x <- [cabalSectionTypeName cabalSectionType]] ++
                              ["build", "build" </> distDir </> "build"]
                    , x <- cabalSourceDirs ++ ["."]]


data Cabal = Cabal
    {cabalName :: PackageName
    ,cabalSections :: [CabalSection]
    } deriving Show

instance Semigroup Cabal where
    Cabal x1 x2 <> Cabal y1 y2 = Cabal (x1?:y1) (x2++y2)

instance Monoid Cabal where
    mempty = Cabal "" []
    mappend = (<>)

data CabalSectionType = Library | Executable String | TestSuite String | Benchmark String
    deriving (Eq,Ord)

cabalSectionTypeName :: CabalSectionType -> Maybe String
cabalSectionTypeName Library = Nothing
cabalSectionTypeName (Executable x) = Just x
cabalSectionTypeName (TestSuite x) = Just x
cabalSectionTypeName (Benchmark x) = Just x

instance Show CabalSectionType where
    show Library = "library"
    show (Executable x) = "exe:" ++ x
    show (TestSuite x) = "test:" ++ x
    show (Benchmark x) = "bench:" ++ x

instance Read CabalSectionType where
    readsPrec _ "library" = [(Library,"")]
    readsPrec _ x
        | Just x <- stripPrefix "exe:" x = [(Executable x, "")]
        | Just x <- stripPrefix "test:" x = [(TestSuite x, "")]
        | Just x <- stripPrefix "bench:" x = [(Benchmark x, "")]
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
    mempty = CabalSection Library "" [] [] [] []
    mappend = (<>)

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
            "benchmark" -> mempty{cabalSections=[parseSection (Benchmark name) xs]}
            _ -> mempty

parseSection typ xs = mempty{cabalSectionType=typ} <> parse xs
    where
        parse = mconcat . map f . parseHanging
        keyValues (x,xs) = let (x1,x2) = word1 x in (lower x1, trimEqual $ filter (not . null) $ x2:xs)
        trimEqual xs = map (drop n) xs
            where n = minimum $ 0 : map (length . takeWhile isSpace) xs
        listSplit = concatMap (wordsBy (`elem` " ,"))

        f (keyValues -> (k,vs)) = case k of
            "if" -> parse vs
            "else" -> parse vs
            "build-depends:" -> mempty{cabalPackages = map (trim . takeWhile (`notElem` "=><")) . splitOn "," $ unwords vs}
            "hs-source-dirs:" -> mempty{cabalSourceDirs=listSplit vs}
            "exposed-modules:" -> mempty{cabalExposedModules=listSplit vs}
            "other-modules:" -> mempty{cabalOtherModules=listSplit vs}
            "main-is:" -> mempty{cabalMainIs=head $ vs ++ [""]}
            _ -> mempty
