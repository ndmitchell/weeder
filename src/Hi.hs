{-# LANGUAGE DeriveGeneric, RecordWildCards, GeneralizedNewtypeDeriving #-}

module Hi(
    HiKey(), Hi(..), Ident(..),
    hiParseDirectory
    ) where

import qualified Data.HashSet as Set
import qualified Data.HashMap.Lazy as Map
import System.FilePath
import System.Directory.Extra
import GHC.Generics
import Data.Tuple.Extra
import Control.Monad
import Data.Char
import Data.Hashable
import Data.List.Extra
import Data.Monoid
import Data.Functor
import Util
import System.IO.Extra
import Prelude

data Ident = Ident {identModule :: ModuleName, identName :: IdentName}
    deriving (Show,Eq,Ord,Generic)
instance Hashable Ident

data Hi = Hi
    {hiModuleName :: ModuleName
        -- ^ Module name
    ,hiImportPackage :: Set.HashSet PackageName
        -- ^ Packages imported by this module
    ,hiExportIdent :: Set.HashSet Ident
        -- ^ Identifiers exported by this module
    ,hiImportIdent :: Set.HashSet Ident
        -- ^ Identifiers used by this module
    ,hiSignatures :: Map.HashMap IdentName (Set.HashSet Ident)
        -- ^ Type signatures of functions defined in this module and the types they refer to
    ,hiFieldName :: Set.HashSet Ident
        -- ^ Things that are field names
    } deriving (Show,Eq,Generic)
instance Hashable Hi

instance Monoid Hi where
    mempty = Hi mempty mempty mempty mempty mempty mempty
    mappend x y = Hi
        {hiModuleName = f (?:) hiModuleName
        ,hiImportPackage = f (<>) hiImportPackage
        ,hiExportIdent = f (<>) hiExportIdent
        ,hiImportIdent = f (<>) hiImportIdent
        ,hiSignatures = f (Map.unionWith (<>)) hiSignatures
        ,hiFieldName = f (<>) hiFieldName
        }
        where f op sel = sel x `op` sel y

-- | Don't expose that we're just using the filename internally
newtype HiKey = HiKey FilePath deriving (Eq,Ord,Hashable)

hiParseDirectory :: FilePath -> IO (Map.HashMap FilePath HiKey, Map.HashMap HiKey Hi)
hiParseDirectory dir = do
    files <- filter ((==) ".dump-hi" . takeExtension) <$> listFilesRecursive dir
    his <- forM files $ \file -> do
        src <- readFile' file
        return (drop (length dir + 1) file, trimSignatures $ hiParseContents src)
    -- here we try and dedupe any identical Hi modules
    let keys = Map.fromList $ map (second HiKey . swap) his
    let mp1 = Map.fromList $ map (second (keys Map.!)) his
    let mp2 = Map.fromList $ map swap $ Map.toList keys
    return (mp1, mp2)

-- note that in some cases we may get more/less internal signatures, so first remove them
trimSignatures :: Hi -> Hi
trimSignatures hi@Hi{..} = hi{hiSignatures = Map.filterWithKey (\k _ -> k `Set.member` names) hiSignatures}
    where names = Set.fromList [s | Ident m s <- Set.toList hiExportIdent, m == hiModuleName]

hiParseContents :: String -> Hi
hiParseContents = mconcat . map f . parseHanging .  lines
    where
        f (x,xs)
            | Just x <- stripPrefix "interface " x = mempty{hiModuleName = parseInterface x}
            | Just x <- stripPrefix "exports:" x = mconcat $ map parseExports xs
            | Just x <- stripPrefix "package dependencies:" x = mempty{hiImportPackage = Set.fromList $ map parsePackDep $ concatMap words $ x:xs}
            | Just x <- stripPrefix "import " x = case xs of
                [] -> mempty -- these are imports of modules from another package, we don't know what is actually used
                xs -> mempty{hiImportIdent = Set.fromList $ map (Ident (words x !! 1) . fst . word1) $ dropWhile ("exports:" `isPrefixOf`) xs}
            | length x == 32, all isHexDigit x,
                (y,ys):_ <- parseHanging xs,
                fun:"::":typ <- concatMap (wordsBy (`elem` ",()[]{} ")) $ y:ys,
                not $ "$" `isPrefixOf` fun =
                mempty{hiSignatures = Map.singleton fun $ Set.fromList $ map parseIdent typ}
            | otherwise = mempty

        -- "old-locale-1.0.0.7@old-locale-1.0.0.7-KGBP1BSKxH5GCm0LnZP04j" -> "old-locale"
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
            where y:ys = map parseIdent $ wordsBy (`elem` "|{} ") x

        -- "Language.Haskell.PPHsMode" -> Ident "Language.Haskell" "PPHsMode"
        parseIdent x
            | isHaskellSymbol $ last x =
                let (a,b) = spanEnd isHaskellSymbol x
                in if null a then Ident "" b else Ident a $ tail b
            | otherwise =
                let (a,b) = breakOnEnd "." x
                in Ident (if null a then "" else init a) b
