{-# LANGUAGE TupleSections, RecordWildCards, ScopedTypeVariables #-}

module Main(main) where

import Hi
import Cabal
import Stack
import Data.List.Extra
import Data.Functor
import Data.Tuple.Extra
import Control.Monad
import System.Exit
import System.IO.Extra
import qualified Data.HashMap.Strict as Map
import System.Directory.Extra
import System.FilePath
import Check
import Warning
import CmdLine
import Prelude



main :: IO ()
main = do
    cmd@Cmd{..} <- getCmd
    errs <- fmap sum $ mapM (weedDirectory cmd) cmdProjects
    when (errs > 0) exitFailure


weedDirectory :: Cmd -> FilePath -> IO Int
weedDirectory Cmd{..} dir = do
    file <- do b <- doesDirectoryExist dir; return $ if b then dir </> "stack.yaml" else dir
    when cmdBuild $ buildStack file
    Stack{..} <- parseStack file
    cabals <- forM stackPackages $ \x -> do
        file <- selectCabalFile $ dir </> x
        (file,) <$> parseCabal file

    fmap sum $ forM cabals $ \(cabalFile, cabal@Cabal{..}) -> do
        (fileToKey, keyToHi) <- hiParseDirectory $ takeDirectory cabalFile </> stackDistDir
        putStrLn $ "= Project " ++ cabalName ++ " ="
        let warn = check (keyToHi Map.!) $ map (id &&& selectHiFiles fileToKey) cabalSections
        putStrLn $ unlines $ showWarningsPretty warn
        return $ length warn
