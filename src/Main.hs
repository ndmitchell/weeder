{-# LANGUAGE TupleSections, RecordWildCards, ScopedTypeVariables #-}

module Main(main) where

import Hi
import Cabal
import Stack
import Data.List.Extra
import Data.Maybe
import Data.Functor
import Data.Tuple.Extra
import Control.Monad
import System.Exit
import System.IO.Extra
import qualified Data.HashMap.Strict as Map
import System.Directory.Extra
import System.FilePath
import System.Environment
import System.Process
import Warnings
import Prelude



main :: IO ()
main = do
    args <- getArgs
    if "--test" `elem` args then
        runTest ("--update" `elem` args)
    else do
        errs <- fmap sum $ mapM weedDirectory $ if null args then ["."] else args
        when (errs > 0) exitFailure


runTest :: Bool -> IO ()
runTest update = do
    _ <- readCreateProcess (proc "stack" ["build"]){cwd=Just "test"} ""
    let f = filter (/= "") . lines
    expect <- readFile' "test/output.txt"
    got <- fmap fst $ captureOutput $ weedDirectory "test"
    if f expect == f got then putStrLn "Test passed" else do
        diff <- findExecutable "diff"
        if update then do
            writeFileBinary "test/output.txt" got
            putStrLn "UPDATED output.txt due to --update"
         else if isNothing diff then
            putStr $ unlines $ map ("- " ++) (lines expect) ++ map ("+ " ++) (lines got)
         else
            withTempDir $ \dir -> do
                writeFile (dir </> "old.txt") expect
                writeFile (dir </> "new.txt") got
                callProcess "diff" ["-u3",dir </> "old.txt", dir </> "new.txt"]
        exitFailure

weedDirectory :: FilePath -> IO Int
weedDirectory dir = do
    file <- do b <- doesDirectoryExist dir; return $ if b then dir </> "stack.yaml" else dir
    Stack{..} <- parseStack file
    cabals <- forM stackPackages $ \x -> do
        file <- selectCabalFile $ dir </> x
        (file,) <$> parseCabal file

    fmap sum $ forM cabals $ \(cabalFile, cabal@Cabal{..}) -> do
        (fileToKey, keyToHi) <- hiParseDirectory $ takeDirectory cabalFile </> stackDistDir
        putStrLn $ "= Project " ++ cabalName ++ " ="
        let warn = warnings (keyToHi Map.!) $ map (id &&& selectHiFiles fileToKey) cabalSections
        if null warn then
            putStrLn "No warnings"
        else
            putStr $ tail $ unlines $ warningTree
                ([\x -> "\n== Section " ++ x ++ " ==",id,("* "++),("  - "++)] ++ repeat id) $
                map warningPath warn
        putStrLn ""
        return $ length warn

warningTree :: Ord a => [a -> a] -> [[a]] -> [a]
warningTree (f:fs) xs = concat
    [ f title : warningTree fs inner
    | (title,inner) <- groupSort $ mapMaybe uncons xs]
