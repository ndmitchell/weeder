{-# LANGUAGE DeriveGeneric, RecordWildCards, GeneralizedNewtypeDeriving, OverloadedStrings #-}

module Hi(
    HiKey(), Hi(..), Ident(..),
    hiParseDirectory
    ) where

import qualified Data.HashSet as Set
import qualified Data.HashMap.Lazy as Map
import System.Console.CmdArgs.Verbosity
import System.FilePath
import System.Directory.Extra
import System.Time.Extra
import GHC.Generics
import Data.Tuple.Extra
import Control.Monad
import Control.Exception
import Control.DeepSeq
import Data.Char
import Data.Hashable
import Data.List.Extra
import Data.Monoid
import Data.Functor
import Util
import qualified Str as S
import System.IO.Extra
import Prelude

data Ident = Ident {identModule :: ModuleName, identName :: IdentName}
    deriving (Show,Eq,Ord,Generic)
instance Hashable Ident
instance NFData Ident

data Hi = Hi
    {hiModuleName :: ModuleName
        -- ^ Module name
    ,hiImportPackage :: Set.HashSet PackageName
        -- ^ Packages imported by this module
    ,hiExportIdent :: Set.HashSet Ident
        -- ^ Identifiers exported by this module
    ,hiImportIdent :: Set.HashSet Ident
        -- ^ Identifiers used by this module
    ,hiImportModule :: Set.HashSet ModuleName
        -- ^ Modules imported and used by this module
        --   Normally equivalent to @Set.map identModule hiImportIdent@, unless a module supplies only instances
    ,hiImportOrphan :: Set.HashSet ModuleName
        -- ^ Orphans that are in scope in this module
    ,hiImportPackageModule :: Set.HashSet (PackageName, ModuleName)
        -- ^ Modules imported from other packages
    ,hiSignatures :: Map.HashMap IdentName (Set.HashSet Ident)
        -- ^ Type signatures of functions defined in this module and the types they refer to
    ,hiFieldName :: Set.HashSet Ident
        -- ^ Things that are field names
    } deriving (Show,Eq,Generic)
instance Hashable Hi
instance NFData Hi

instance Monoid Hi where
    mempty = Hi mempty mempty mempty mempty mempty mempty mempty mempty mempty
    mappend x y = Hi
        {hiModuleName = f (?:) hiModuleName
        ,hiImportPackage = f (<>) hiImportPackage
        ,hiExportIdent = f (<>) hiExportIdent
        ,hiImportIdent = f (<>) hiImportIdent
        ,hiImportModule = f (<>) hiImportModule
        ,hiImportPackageModule = f (<>) hiImportPackageModule
        ,hiImportOrphan = f (<>) hiImportOrphan
        ,hiSignatures = f (Map.unionWith (<>)) hiSignatures
        ,hiFieldName = f (<>) hiFieldName
        }
        where f op sel = sel x `op` sel y

-- | Don't expose that we're just using the filename internally
newtype HiKey = HiKey FilePath deriving (Eq,Ord,Hashable)

hiParseDirectory :: FilePath -> IO (Map.HashMap FilePath HiKey, Map.HashMap HiKey Hi)
hiParseDirectory dir = do
    whenLoud $ putStrLn $ "Reading hi directory " ++ dir
    files <- filter ((==) ".dump-hi" . takeExtension) <$> listFilesRecursive dir
    his <- forM files $ \file -> do
        let name = drop (length dir + 1) file
        whenLoud $ do
            putStr $ "Reading hi file " ++ name ++ " ... "
            hFlush stdout
        (time, (len, res)) <- duration $ do
            src <- S.readFileUTF8 file
            len <- evaluate $ S.length src
            let res = trimSignatures $ hiParseContents src
            evaluate $ rnf res
            return (len, res)
        whenLoud $ putStrLn $ S.showLength len ++ " bytes in " ++ showDuration time
        return (name, res)
    -- here we try and dedupe any identical Hi modules
    let keys = Map.fromList $ map (second HiKey . swap) his
    mp1 <- evaluate $ Map.fromList $ map (second (keys Map.!)) his
    mp2 <- evaluate $ Map.fromList $ map swap $ Map.toList keys
    whenLoud $ putStrLn $ "Found " ++ show (Map.size mp1) ++ " files, " ++ show (Map.size mp2) ++ " distinct"
    return (mp1, mp2)

-- note that in some cases we may get more/less internal signatures, so first remove them
trimSignatures :: Hi -> Hi
trimSignatures hi@Hi{..} = hi{hiSignatures = Map.filterWithKey (\k _ -> k `Set.member` names) hiSignatures}
    where names = Set.fromList [s | Ident m s <- Set.toList hiExportIdent, m == hiModuleName]

hiParseContents :: Str -> Hi
hiParseContents = mconcat . map f . parseHanging2 . S.linesCR
    where
        f (x,xs)
            | Just x <- S.stripPrefix "interface " x = mempty{hiModuleName = parseInterface $ S.toList x}
            | Just x <- S.stripPrefix "exports:" x = mconcat $ map (parseExports . S.toList) $ unindent2 xs
            | Just x <- S.stripPrefix "orphans:" x = mempty{hiImportOrphan = Set.fromList $ map parseInterface $ concatMap (words . S.toList) $ x:xs}
            | Just x <- S.stripPrefix "package dependencies:" x = mempty{hiImportPackage = Set.fromList $ map parsePackDep $ concatMap (words . S.toList) $ x:xs}
            | Just x <- S.stripPrefix "import " x = case unindent2 xs of
                [] | (pkg, mod) <- breakOn ":" $ words (S.toList x) !! 1 -> mempty
                    {hiImportPackageModule = Set.singleton (parsePackDep pkg, drop 1 mod)}
                xs -> let m = words (S.toList x) !! 1 in mempty
                    {hiImportModule = Set.singleton m
                    ,hiImportIdent = Set.fromList $ map (Ident m . fst . word1 . S.toList) $ dropWhile ("exports:" `S.isPrefixOf`) xs}
            | S.length x == S.ugly 32, S.all isHexDigit x,
                (y,ys):_ <- parseHanging2 $ map (S.drop $ S.ugly 2) xs,
                fun:"::":typ <- concatMap (wordsBy (`elem` (",()[]{} " :: String)) . S.toList) $ y:ys,
                not $ "$" `isPrefixOf` fun =
                mempty{hiSignatures = Map.singleton fun $ Set.fromList $ map parseIdent typ}
            | otherwise = mempty

        -- "old-locale-1.0.0.7@old-locale-1.0.0.7-KGBP1BSKxH5GCm0LnZP04j" -> "old-locale"
        -- "old-locale-1.0.0.7" -> "old-locale"
        parsePackDep = intercalate "-" . takeWhile (any isAlpha) . wordsBy (== '-') . takeWhile (/= '@')

        -- "hlint-1.9.41-IPKy9tGF1918X9VRp9DMhp:HSE.All 8002" -> "HSE.All"
        parseInterface = drop 1 . snd . breakOn ":" . fst . word1

        -- "Apply.applyHintFile"
        -- "Language.Haskell.PPHsMode{Language.Haskell.PPHsMode caseIndent}
        -- Return the identifiers and the fields. Fields are never qualified but everything else is.
        parseExports x = mempty
            {hiExportIdent = Set.fromList $ y : [Ident (a ?: identModule y) b | Ident a b <- ys]
            ,hiFieldName = Set.fromList [Ident (identModule y) b | Ident "" b <- ys]
            ,hiSignatures = Map.fromList [(b, Set.singleton y) | Ident _ b <- ys, b /= identName y]
            }
            where y:ys = map parseIdent $ wordsBy (`elem` ("{} " :: String)) x

        -- "Language.Haskell.PPHsMode" -> Ident "Language.Haskell" "PPHsMode"
        parseIdent x
            | isHaskellSymbol $ last x =
                let (a,b) = spanEnd isHaskellSymbol x
                in if null a then Ident "" b else Ident a $ tail b
            | otherwise =
                let (a,b) = breakOnEnd "." x
                in Ident (if null a then "" else init a) b
