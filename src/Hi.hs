{-# LANGUAGE DeriveGeneric, RecordWildCards #-}

module Hi(
    Hi(..), Ident(..),
    parseHi,
    hiExportIdentUnsupported
    ) where

import qualified Data.HashSet as Set
import qualified Data.HashMap.Lazy as Map
import GHC.Generics
import Data.Char
import Data.Hashable
import Data.List.Extra
import Data.Monoid
import Data.Functor
import Util
import System.IO.Extra
import Prelude

data Ident = Ident {identModule :: String, identName :: String}
    deriving (Show,Eq,Ord,Generic)
instance Hashable Ident

data Hi = Hi
    {hiFileName :: FilePath
        -- ^ File the Hi file was read from
    ,hiModuleName :: String
        -- ^ Module name
    ,hiImportPackage :: Set.HashSet PackageName
        -- ^ Packages imported by this module
    ,hiExportIdent :: Set.HashSet Ident
        -- ^ Identifiers exported by this module
    ,hiImportIdent :: Set.HashSet Ident
        -- ^ Identifiers used by this module
    ,hiSignatures :: Map.HashMap String (Set.HashSet Ident)
        -- ^ Type signatures of functions defined in this module and the types they refer to
    ,hiFieldName :: Set.HashSet Ident
        -- ^ Things that are field names
    } deriving (Show,Eq,Generic)
instance Hashable Hi

instance Monoid Hi where
    mempty = Hi "" "" mempty mempty mempty mempty mempty
    mappend (Hi x1 x2 x3 x4 x5 x6 x7) (Hi y1 y2 y3 y4 y5 y6 y7) =
        Hi (x1 ?: y1) (x2 ?: y2) (x3 <> y3) (x4 <> y4) (x5 <> y5)
            (Map.unionWith (<>) x6 y6) (x7 <> y7)

-- | Things that are exported and aren't of use if they aren't used. Don't worry about:
--
-- * Types that are exported and used in a definition that is exported.
-- * Field selectors that aren't used but where the constructor is used (handy documentation).
hiExportIdentUnsupported :: Hi -> Set.HashSet Ident
hiExportIdentUnsupported Hi{..} = (hiExportIdent `Set.difference` supported) `Set.difference` hiFieldName
    where supported = Set.unions [v | (k,v) <- Map.toList hiSignatures, k `Set.member` names]
          names = Set.fromList [s | Ident m s <- Set.toList hiExportIdent, m == hiModuleName]

parseHi :: FilePath -> IO Hi
parseHi file = do
    hi <- parse <$> readFile' file
    return hi{hiFileName=file}

parse = mconcat . map f . parseHanging .  lines
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
                fun:"::":typ <- concatMap (wordsBy (`elem` ",[]{} ")) $ y:ys,
                not $ "$" `isPrefixOf` fun =
                mempty{hiSignatures = Map.singleton (unbracket fun) $ Set.fromList $ map parseIdent typ}
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
            ,hiSignatures = Map.fromList [(b, Set.singleton y) | Ident _ b <- ys]
            }
            where y:ys = map parseIdent $ wordsBy (`elem` "{} ") x

        -- "Language.Haskell.PPHsMode" -> Ident "Language.Haskell" "PPHsMode"
        parseIdent x
            | isHaskellSymbol $ last x =
                let (a,b) = spanEnd isHaskellSymbol x
                in if null a then Ident "" b else Ident a $ tail b
            | otherwise =
                let (a,b) = breakOnEnd "." x
                in Ident (if null a then "" else init a) b

unbracket :: String -> String
unbracket ('(':x) | Just x <- stripSuffix ")" x = x
unbracket x = x
