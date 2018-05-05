module Main(main) where

import Weeder
import System.Exit
import System.Environment
import Control.Monad

main :: IO ()
main = do
    bad <- weeder =<< getArgs
    when (bad > 0) exitFailure
