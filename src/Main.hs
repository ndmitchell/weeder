{-# LANGUAGE TupleSections, RecordWildCards #-}

module Main(main) where

import Hi
import Cabal
import Stack
import Util
import Data.List.Extra
import Data.Maybe
import Data.Functor
import Control.Monad
import System.Exit
import System.IO.Extra
import qualified Data.HashMap.Strict as Map
import qualified Data.HashSet as Set
import System.Directory.Extra
import System.FilePath
import System.Environment
import System.Process
import Prelude


main :: IO ()
main = do
    args <- getArgs
    if args == ["--test"] then
        runTest
    else
        mapM_ weedDirectory $ if null args then ["."] else args


runTest :: IO ()
runTest = do
    _ <- readCreateProcess (proc "stack" ["build"]){cwd=Just "test"} ""
    let f = filter (/= "") . lines
    expect <- readFile' "test/output.txt"
    got <- fmap fst $ captureOutput $ weedDirectory "test"
    if f expect == f got then putStrLn "Test passed" else do
        diff <- findExecutable "diff"
        if isNothing diff then
            putStr $ unlines $ map ("- " ++) (lines expect) ++ map ("+ " ++) (lines got)
         else do
            withTempDir $ \dir -> do
                writeFile (dir </> "old.txt") expect
                writeFile (dir </> "new.txt") got
                callProcess "diff" ["-u3",dir </> "old.txt", dir </> "new.txt"]
        exitFailure

listLines = map ("  "++) . sort

weedDirectory :: FilePath -> IO ()
weedDirectory dir = do
    dir <- return $ if takeFileName dir == "stack.yaml" then takeDirectory dir else dir
    dir <- canonicalizePath dir
    -- the distDir is relative to each .cabal file directory
    distSuffix <- fst . line1 <$> readCreateProcess (proc "stack" ["path","--dist-dir"]){cwd=Just dir} ""

    Stack{..} <- parseStack $ dir </> "stack.yaml"
    cabals <- forM stackPackages $ \x -> do
        file <- selectCabalFile $ dir </> x
        (file,) <$> parseCabal file

    forM_ cabals $ \(cabalFile, cabal@Cabal{..}) -> do
        let distDir = takeDirectory cabalFile </> distSuffix
        his <- listFilesRecursive distDir
        his <- Map.fromList <$> sequence [(drop (length distDir + 1) x,) <$> parseHi x | x <- his, takeExtension x == ".dump-hi"]
 
        forM_  cabalSections $ \sect@CabalSection{..} -> do
            putStrLn $ "== Weeding " ++ cabalName ++ ", " ++ cabalSectionLabel sect ++ " =="

            -- first go looking for packages that are not used
            let (external, internal) = findHis his sect
            let bad = Set.fromList cabalPackages `Set.difference` Set.unions (map hiImportPackage $ external ++ internal)
            if Set.null bad then
                putStrLn "No weeds in the build-depends field"
            else
                putStr $ unlines $ "Redundant build-depends entries:" : listLines (Set.toList bad)

            -- now see which things are defined in and exported out of the internals, but not used elsewhere or external
            let publicAPI = Set.unions $ map hiExportIdentUnsupported external
            let visibleInternals = Set.unions [Set.filter ((==) hiModuleName . identModule) $ hiExportIdentUnsupported hi | hi@Hi{..} <- internal]
            -- if someone imports and exports something assume that isn't also a use (find some redundant warnings)
            let usedAnywhere = Set.unions [hiImportIdent `Set.difference` hiExportIdent | Hi{..} <- external ++ internal]
            let bad = visibleInternals `Set.difference` Set.union publicAPI usedAnywhere
            if Set.null bad then
                putStrLn "No weeds in the module exports"
            else
                putStr $ unlines $ concat
                    [ ("Weeds exported from " ++ m) : listLines is
                    | (m, is) <- groupSort [(m,i) | Ident m i <- Set.toList bad]]
            putStrLn ""


-- (exposed, internal)
findHis :: Map.HashMap FilePath Hi -> CabalSection -> ([Hi], [Hi])
findHis his sect@CabalSection{..} = (external, internal)
    where
        external = [findHi his sect $ Left cabalMainIs | cabalMainIs /= ""] ++
                   [findHi his sect $ Right x | x <- cabalExposedModules]
        internal = [findHi his sect $ Right x | x <- filter (not . isPaths) cabalOtherModules]

isPaths = isPrefixOf "Paths_"

findHi :: Map.HashMap FilePath Hi -> CabalSection -> Either FilePath ModuleName -> Hi
findHi his CabalSection{..} name = fromMaybe err $ firstJust (`Map.lookup` his) poss
    where
        err = error $ "Failed to find Hi file when looking for " ++ show name ++ " " ++ show (Map.keys his, poss)
        poss = [ normalise $ joinPath (root : x : either (return . dropExtension) (splitOn ".") name) <.> "dump-hi"
               | root <- ["build" </> cabalSectionName </> (cabalSectionName ++ "-tmp") | cabalSectionName /= ""] ++ ["build"]
               , x <- if null cabalSourceDirs then ["."] else cabalSourceDirs]
