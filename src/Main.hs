{-# LANGUAGE TupleSections, RecordWildCards, ScopedTypeVariables #-}

module Main(main) where

import Hi
import Cabal
import Stack
import Data.List.Extra
import Data.Functor
import Data.Tuple.Extra
import Control.Monad.Extra
import System.Exit
import System.IO.Extra
import qualified Data.HashMap.Strict as Map
import System.Directory.Extra
import System.FilePath
import Check
import Warning
import CmdLine
import Prelude


data Result = Good | Bad deriving Eq

main :: IO ()
main = do
    cmd@Cmd{..} <- getCmd
    res <- mapM (weedDirectory cmd) cmdProjects
    when (Bad `elem` res) exitFailure


weedDirectory :: Cmd -> FilePath -> IO Result
weedDirectory Cmd{..} dir = do
    file <- do b <- doesDirectoryExist dir; return $ if b then dir </> "stack.yaml" else dir
    when cmdBuild $ buildStack file
    Stack{..} <- parseStack cmdDistDir file
    cabals <- forM stackPackages $ \x -> do
        file <- selectCabalFile $ dir </> x
        (file,) <$> parseCabal file

    ignore <- do
        let x = takeDirectory file </> ".weeder.yaml"
        b <- doesFileExist x
        if not b then return [] else readWarningsFile x
    let quiet = cmdJson || cmdYaml

    res <- forM cabals $ \(cabalFile, Cabal{..}) -> do
        (fileToKey, keyToHi) <- hiParseDirectory $ takeDirectory cabalFile </> stackDistDir
        let full = check (keyToHi Map.!) cabalName $
                   map (id &&& selectHiFiles stackDistDir fileToKey) cabalSections
        let warn = if cmdShowAll || cmdMatch then full else ignoreWarnings ignore full
        unless quiet $
            putStrLn $ unlines $ showWarningsPretty cabalName warn
        return (length full - length warn, warn)
    let (ignored, warns) = sum *** concat $ unzip res

    when cmdJson $ putStrLn $ showWarningsJson warns
    when cmdYaml $ putStrLn $ showWarningsYaml warns
    if cmdMatch then
        if sort ignore == sort warns then do
            putStrLn "Warnings match"
            return Good
        else do
            putStrLn "MISSING WARNINGS"
            putStrLn $ unlines $ showWarningsPretty "" $ ignore \\ warns
            putStrLn "EXTRA WARNINGS"
            putStrLn $ unlines $ showWarningsPretty "" $ warns \\ ignore
            return Bad
     else do
        when (ignored > 0 && not quiet) $
            putStrLn $ "Ignored " ++ show ignored ++ " weeds (pass --show-all to see them)"
        return $ if null warns then Good else Bad
