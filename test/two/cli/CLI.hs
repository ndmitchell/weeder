module CLI (parseArgs, sample) where

import Common
import System.Console.CmdArgs

sample = Sample{hello = mempty &= help "World argument" &= opt "world"}
         &= summary "Sample v1"

parseArgs :: IO Sample
parseArgs = cmdArgs sample
