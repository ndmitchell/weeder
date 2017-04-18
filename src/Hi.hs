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
import Util
import System.IO.Extra
import Prelude

data Ident = Ident {identModule :: String, identName :: String}
    deriving (Show,Eq,Generic)
instance Hashable Ident

data Hi = Hi
    {hiModuleName :: String
    ,hiImportPackage :: Set.HashSet PackageName
    ,hiExportIdent :: Set.HashSet Ident
    ,hiImportIdent :: Set.HashSet Ident
    ,hiSignatures :: Map.HashMap String (Set.HashSet Ident)
    } deriving Show

instance Monoid Hi where
    mempty = Hi "" Set.empty Set.empty Set.empty Map.empty
    mappend (Hi x1 x2 x3 x4 x5) (Hi y1 y2 y3 y4 y5) =
        Hi (x1 ?: y1) (Set.union x2 y2) (Set.union x3 y3) (Set.union x4 y4) (Map.unionWith Set.union x5 y5)

hiExportIdentUnsupported :: Hi -> Set.HashSet Ident
hiExportIdentUnsupported Hi{..} = hiExportIdent `Set.difference` supported
    where supported = Set.unions [v | (k,v) <- Map.toList hiSignatures, k `Set.member` names]
          names = Set.fromList [s | Ident m s <- Set.toList hiExportIdent, m == hiModuleName]

parseHi :: FilePath -> IO Hi
parseHi fp = fmap (parse fp) . readFile' $ fp

parse fp = mconcat . map f . parseHanging .  lines
    where
        f (x,xs)
            | Just x <- stripPrefix "interface " x = mempty{hiModuleName = parseInterface x}
            | Just x <- stripPrefix "exports:" x = mempty{hiExportIdent = Set.fromList $ concatMap parseExports xs}
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
        parseExports x = y : [Ident (a ?: identModule y) b | Ident a b <- ys]
            where y:ys = map parseIdent $ wordsBy (`elem` "{} ") x

        -- "Language.Haskell.PPHsMode" -> Ident "Language.Haskell" "PPHsMode"
        parseIdent x
            | isSymbol $ last x = let (a,b) = spanEnd isSymbol x
                                  in if null a then Ident "" b else Ident a $ tail b
            | otherwise = let (a,b) = breakOnEnd "." x
                          in Ident (if null a then "" else init a) b


unbracket :: String -> String
unbracket ('(':x) | Just x <- stripSuffix ")" x = x
unbracket x = x
