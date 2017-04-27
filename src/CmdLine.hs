{-# LANGUAGE DeriveDataTypeable #-}
{-# OPTIONS_GHC -fno-warn-missing-fields -fno-cse -O0 #-}

module CmdLine(
    Cmd(..), getCmd
    ) where

import System.Console.CmdArgs.Implicit
import Paths_weeder
import Data.Version


data Cmd = Cmd
    {cmdProjects :: [FilePath]
    ,cmdBuild :: Bool
    ,cmdTest :: Bool
    ,cmdMatch :: Bool
    ,cmdJson :: Bool
    ,cmdYaml :: Bool
    ,cmdShowAll :: Bool
    } deriving (Show, Data, Typeable)

getCmd :: IO Cmd
getCmd = automatic <$> cmdArgsRun mode

automatic :: Cmd -> Cmd
automatic cmd
    | cmdTest cmd = cmd{cmdTest=False,cmdProjects=["test"],cmdBuild=True,cmdMatch=True}
    | null $ cmdProjects cmd = cmd{cmdProjects=["."]}
    | otherwise = cmd 

mode :: Mode (CmdArgs Cmd)
mode = cmdArgsMode $ Cmd
    {cmdProjects = def &= args &= typ "DIR"
    ,cmdBuild = nam "build" &= help "Build the project first"
    ,cmdTest = nam "test" &= help "Run the test suite"
    ,cmdMatch = nam "match" &= help "Make the .weeder.yaml perfectly match"
    ,cmdJson = nam "json" &= help "Output JSON"
    ,cmdYaml = nam "yaml" &= help "Output YAML"
    ,cmdShowAll = nam "show-all" &= help "Show even ignored warnings"
    } &= explicit &= name "weeder" &= verbosity
    &= summary ("Weeder v" ++ showVersion version ++ ", (C) Neil Mitchell 2017")
    where
        nam xs = def &= explicit &= name xs &= name [head xs]
