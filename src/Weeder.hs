{-# LANGUAGE TupleSections, RecordWildCards, ScopedTypeVariables #-}

module Weeder(weeder) where

import Hi
import Cabal
import Stack
import Data.List.Extra
import Data.Functor
import Data.Tuple.Extra
import Control.Monad.Extra
import System.Console.CmdArgs.Verbosity
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

weeder :: IO Bool
weeder = do
    cmd@Cmd{..} <- getCmd
    res <- mapM (weedPath cmd) cmdProjects
    return (Bad `elem` res)


weedPath :: Cmd -> FilePath -> IO Result
weedPath Cmd{..} proj = do
    -- project may either be a directory name, or a stack.yaml file
    file <- do
        isDir <- doesDirectoryExist proj
        if isDir then findStack proj else return proj
    whenLoud $ putStrLn $ "Running on Stack file " ++ file
    when cmdBuild $ buildStack file
    Stack{..} <- parseStack cmdDistDir file
    cabals <- forM stackPackages $ \x -> do
        file <- selectCabalFile x
        (file,) <$> parseCabal file

    ignore <- do
        let x = takeDirectory file </> ".weeder.yaml"
        b <- doesFileExist x
        if not b then return [] else do
            whenLoud $ putStrLn $ "Reading ignored warnings from " ++ x
            readWarningsFile x
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
