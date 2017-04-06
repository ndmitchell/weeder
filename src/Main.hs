{-# LANGUAGE TupleSections, RecordWildCards #-}

module Main(main) where

import Hi
import Cabal
import Util
import Data.List.Extra
import Data.Tuple.Extra
import Data.Maybe
import Control.Monad
import System.Directory.Extra
import System.FilePath
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

    let pickAndParse ext parse files = sequence [(x,) <$> parse x | x <- files, takeExtension x == ext]
    his <- fmap (map (first $ drop $ length distDir + 1)) $ pickAndParse ".dump-hi" parseHi =<< listFilesRecursive distDir
    cabals <- pickAndParse ".cabal" parseCabal =<< listFiles dir
 
    -- first go looking for packages that are not used
    forM_ cabals $ \(cabalFile, cabal@Cabal{..}) ->
        forM_  cabalSections $ \sect@CabalSection{..} -> do
            -- find all Hi files that it is responsible for
            let files = [findHi his sect $ Right x | x <- delete ("Paths_" ++ cabalName) $ cabalExposedModules ++ cabalOtherModules] ++
                        [findHi his sect $ Left cabalMainIs | cabalMainIs /= ""]
            let bad = cabalPackages \\ nubOrd (concatMap hiImportPackage files)
            print ("Weed packages", cabalSectionLabel sect, bad)


findHi :: [(FilePath, Hi)] -> CabalSection -> Either FilePath ModuleName -> Hi
findHi files CabalSection{..} name = fromMaybe err $ firstJust (`lookup` files) poss
    where
        err = error $ "Failed to find Hi file when looking for " ++ show name ++ " " ++ show (map fst files, poss)
        root = if null cabalSectionName then "build" else "build" </> cabalSectionName </> (cabalSectionName ++ "-tmp")
        poss = [ normalise $ joinPath (root : x : either (pure . dropExtension) (splitOn ".") name) <.> "dump-hi"
               | x <- if null cabalSourceDirs then ["."] else cabalSourceDirs]

 {-
    his <- mapM parseHi dumpHis
    let hi = mconcat his
    let importPackage = Set.fromList $ hiImportPackage hi
    let exportIdent = Set.fromList $ hiExportIdent hi
    let importIdent = Set.fromList $ hiImportIdent hi
    print $ explicitPackage `Set.difference` importPackage
    let ignore x = "Language.Haskell.Exts." `isPrefixOf` x || '{' `elem` x || '}' `elem` x || '.' `notElem` x
    putStr $ unlines $ filter (not . ignore) $ Set.toList $ exportIdent `Set.difference` importIdent

explicitPackage = Set.fromList $ words "base process filepath directory containers unordered-containers yaml vector text bytestring transformers cpphs cmdargs haskell-src-exts uniplate ansi-terminal extra js-flot refact"
-}
