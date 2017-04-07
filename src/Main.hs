{-# LANGUAGE TupleSections, RecordWildCards #-}

module Main(main) where

import Hi
import Cabal
import Stack
import Util
import Data.List.Extra
import Data.Maybe
import Control.Monad
import qualified Data.HashMap.Strict as Map
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
    dir <- return $ if takeFileName dir == "stack.yaml" then takeDirectory dir else dir
    dir <- canonicalizePath dir
    putStrLn $ "== Weeding " ++ dir ++ " =="
    distDir <- (dir </>) . fst . line1 . fromStdout <$> cmd (Cwd dir) "stack path --dist-dir"

    Stack{..} <- parseStack $ dir </> "stack.yaml"
    cabals <- forM stackPackages $ \x -> parseCabal =<< selectCabalFile (dir </> x)
    his <- listFilesRecursive distDir
    his <- fmap Map.fromList $ sequence [(drop (length distDir + 1) x,) <$> parseHi x | x <- his, takeExtension x == ".dump-hi"]
 
    -- first go looking for packages that are not used
    forM_ cabals $ \cabal@Cabal{..} ->
        forM_  cabalSections $ \sect@CabalSection{..} -> do
            -- find all Hi files that it is responsible for
            let (external, internal) = findHis his sect
            let bad = cabalPackages \\ nubOrd (concatMap hiImportPackage $ external ++ internal)
            print ("Weed packages", cabalSectionLabel sect, bad)

    -- next try and find exports that aren't used
    -- given a function, it is in one of N states:
    -- not used, only in tests


-- (exposed, internal)
findHis :: Map.HashMap FilePath Hi -> CabalSection -> ([Hi], [Hi])
findHis his sect@CabalSection{..} = (external, internal)
    where
        external = [findHi his sect $ Left cabalMainIs | cabalMainIs /= ""] ++
                   [findHi his sect $ Right x | x <- cabalExposedModules]
        internal = [findHi his sect $ Right x | x <- filter (not . isPrefixOf "Paths_") cabalOtherModules]


findHi :: Map.HashMap FilePath Hi -> CabalSection -> Either FilePath ModuleName -> Hi
findHi his CabalSection{..} name = fromMaybe err $ firstJust (`Map.lookup` his) poss
    where
        err = error $ "Failed to find Hi file when looking for " ++ show name ++ " " ++ show (Map.keys his, poss)
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
