
module Main(main) where

import System.IO.Extra
import Data.List.Extra
import Data.Tuple.Extra
import Data.Char
import System.Directory.Extra
import System.FilePath
import qualified Data.HashSet as Set


main = do
    files <- listFilesRecursive "C:/Neil/hlint/.stack-work/dist/ca59d0ab/build/src/"
    files <- return $ filter ((==) ".dump-hi" . takeExtension) files
    his <- mapM (fmap parseHI . readFile') files
    let hi = mconcat his
    let importPackage = Set.fromList $ hiImportPackage hi
    let exportIdent = Set.fromList $ hiExportIdent hi
    let importIdent = Set.fromList $ hiImportIdent hi
    print $ explicitPackage `Set.difference` importPackage
    let ignore x = "Language.Haskell.Exts." `isPrefixOf` x || '{' `elem` x || '}' `elem` x || '.' `notElem` x
    putStr $ unlines $ filter (not . ignore) $ Set.toList $ exportIdent `Set.difference` importIdent

explicitPackage = Set.fromList $ words "base process filepath directory containers unordered-containers yaml vector text bytestring transformers cpphs cmdargs haskell-src-exts uniplate ansi-terminal extra js-flot refact"

data HI = HI
    {hiExportIdent :: [String]
    ,hiImportPackage :: [String]
    ,hiImportModule :: [String]
    ,hiImportIdent :: [String]
    } deriving Show

instance Monoid HI where
    mempty = HI [] [] [] []
    mappend (HI x1 x2 x3 x4) (HI y1 y2 y3 y4) = HI (x1++y1) (x2++y2) (x3++y3) (x4++y4)

parseHI :: String -> HI
parseHI = foldMap f . repeatedly (\(x:xs) -> first (x:) $ span (" " `isPrefixOf`) xs) .  lines
    where
        f (x:xs) | Just x <- stripPrefix "exports:" x = mempty{hiExportIdent=concatMap words $ x:xs}
        f (x:xs) | Just x <- stripPrefix "module dependencies:" x = mempty{hiImportModule=concatMap words $ x:xs}
        f (x:xs) | Just x <- stripPrefix "package dependencies:" x = mempty{hiImportPackage=map cleanupPackage $ concatMap words $ x:xs}
        f (x:xs) | Just x <- stripPrefix "import " x = case xs of
            [] -> mempty{hiImportIdent = [words x !! 1]}
            _:xs -> mempty{hiImportIdent = map (\y -> (words x !! 1) ++ "." ++ fst (word1 y)) xs}
        f _ = mempty

        -- "old-locale-1.0.0.7@old-locale-1.0.0.7-KGBP1BSKxH5GCm0LnZP04j" -> "old-locale"
        cleanupPackage = intercalate "-" . takeWhile (any isAlpha) . wordsBy (== '-') . takeWhile (/= '@')
