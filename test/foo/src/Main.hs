
module Main(main) where

import Library1

main = exported `seq` print 1
