{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Hi(Hi(..), parseHi) where

import qualified Data.HashSet as Set
import Data.Char
import Data.Hashable
import Data.List.Extra
import Util
import System.IO.Extra

newtype Ident = Ident String
    deriving (Show,Eq,Hashable)

data Hi = Hi
    {hiImportPackage :: Set.HashSet PackageName
    ,hiExportIdent :: Set.HashSet Ident
    ,hiImportIdent :: Set.HashSet Ident
    } deriving Show

instance Monoid Hi where
    mempty = Hi Set.empty Set.empty Set.empty
    mappend (Hi x1 x2 x3) (Hi y1 y2 y3) = Hi (Set.union x1 y1) (Set.union x2 y2) (Set.union x3 y3)

parseHi :: String -> IO Hi
parseHi = fmap parse . readFile'

parse = foldMap f . parseHanging .  lines
    where
        f (x,xs)
            | Just x <- stripPrefix "exports:" x = mempty{hiExportIdent = Set.fromList $ map Ident $ concatMap words $ x:xs}
            | Just x <- stripPrefix "package dependencies:" x = mempty{hiImportPackage = Set.fromList $ map cleanupPackage $ concatMap words $ x:xs}
            | Just x <- stripPrefix "import " x = case xs of
                [] -> mempty{hiImportIdent = Set.singleton $ Ident $ words x !! 1}
                _:xs -> mempty{hiImportIdent = Set.fromList $ map (\y -> Ident $ (words x !! 1) ++ "." ++ fst (word1 y)) xs}
            | otherwise = mempty

        -- "old-locale-1.0.0.7@old-locale-1.0.0.7-KGBP1BSKxH5GCm0LnZP04j" -> "old-locale"
        cleanupPackage = intercalate "-" . takeWhile (any isAlpha) . wordsBy (== '-') . takeWhile (/= '@')
