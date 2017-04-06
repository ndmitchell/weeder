
module Main(main) where

import Hi
import System.IO.Extra
import Data.List
import System.Directory.Extra
import System.FilePath
import qualified Data.HashSet as Set
import System.Environment
import Development.Shake.Command

main :: IO ()
main = do
    args <- getArgs
    mapM_ weedDirectory $ if null args then ["."] else args

weedDirectory :: FilePath -> IO ()
weedDirectory dir = do
    dir <- canonicalizePath dir
    putStrLn $ "== Weeding " ++ dir ++ " =="
    distDir <- (dir </>) . takeWhile (/= '\n') . fromStdout <$> cmd (Cwd dir) "stack path --dist-dir"
    print distDir
    dumpHis <- filter ((==) ".dump-hi" . takeExtension) <$> listFilesRecursive distDir
    his <- mapM (fmap parseHi . readFile') dumpHis
    let hi = mconcat his
    let importPackage = Set.fromList $ hiImportPackage hi
    let exportIdent = Set.fromList $ hiExportIdent hi
    let importIdent = Set.fromList $ hiImportIdent hi
    print $ explicitPackage `Set.difference` importPackage
    let ignore x = "Language.Haskell.Exts." `isPrefixOf` x || '{' `elem` x || '}' `elem` x || '.' `notElem` x
    putStr $ unlines $ filter (not . ignore) $ Set.toList $ exportIdent `Set.difference` importIdent

explicitPackage = Set.fromList $ words "base process filepath directory containers unordered-containers yaml vector text bytestring transformers cpphs cmdargs haskell-src-exts uniplate ansi-terminal extra js-flot refact"
