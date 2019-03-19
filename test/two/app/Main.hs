module Main where

import qualified Data.Text.IO as T
import CLI (parseArgs)
import Common

main :: IO ()
main = T.putStrLn =<< fmap (helloMessage . hello) parseArgs
