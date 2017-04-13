
module Dir.Everything(module Data.List.Extra, module Dir.Everything) where

import Data.List.Extra

usedFunction1 = 12 :: Int
usedFunction2 = Other
unusedFunction = 12 :: Int
(=~=) a b = a == b -- used
(==~==) a b = a == b

data Other = Other

data Data
    = Ctor1 {field1 :: Int, field2 :: Int, field3 :: Int}
    | Ctor2 String

type Type = Data
