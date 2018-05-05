module Main(main) where

import Weeder
import System.Exit
import Control.Monad

main :: IO ()
main = do
    success <- weeder
    unless success exitFailure
