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
import qualified Data.HashSet as Set
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
    distDir <- (dir </>) . fst . line1 . fromStdout <$> cmd (Cwd dir) "stack path --dist-dir"

    Stack{..} <- parseStack $ dir </> "stack.yaml"
    cabals <- forM stackPackages $ \x -> parseCabal =<< selectCabalFile (dir </> x)
    his <- listFilesRecursive distDir
    his <- fmap Map.fromList $ sequence [(drop (length distDir + 1) x,) <$> parseHi x | x <- his, takeExtension x == ".dump-hi"]
 
    forM_ cabals $ \cabal@Cabal{..} ->
        forM_  cabalSections $ \sect@CabalSection{..} -> do
            putStrLn $ "== Weeding " ++ cabalName ++ ", " ++ cabalSectionLabel sect ++ " =="

            -- first go looking for packages that are not used
            let (external, internal) = findHis his sect
            let bad = Set.fromList cabalPackages `Set.difference` Set.unions (map hiImportPackage $ external ++ internal)
            if Set.null bad then
                putStrLn "No weeds in the build-depends field"
            else
                putStr $ unlines $ "Redundant build-depends entries:" : map ("  "++) (Set.toList bad)

            -- now see which things are defined in and exported out of the internals, but not used elsewhere or external
            let publicAPI = Set.unions $ map hiExportIdent external
            let visibleInternals = Set.unions [Set.filter ((==) hiModuleName . identModule) hiExportIdent | Hi{..} <- internal]
            -- if someone imports and exports something assume that isn't also a use (find some redundant warnings)
            let usedAnywhere = Set.unions [hiImportIdent `Set.difference` hiExportIdent | Hi{..} <- external ++ internal]
            let bad = visibleInternals `Set.difference` Set.union publicAPI usedAnywhere
            if Set.null bad then
                putStrLn "No weeds in the module exports"
            else
                putStr $ unlines $ concat
                    [ ("Weeds exported from " ++ m) : map ("  "++) is
                    | (m, is) <- groupSort [(m,i) | Ident m i <- Set.toList bad]]
            putStrLn ""


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
