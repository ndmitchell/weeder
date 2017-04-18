
module Dir.Everything(module Data.List.Extra, module Dir.Everything) where

import Data.List.Extra

usedFunction1 = 12 :: Int
usedFunction2 = Other
unusedFunction = 12 :: Int
(=~=) a b = a == b -- used
(==~==) a b = a == b

data D1 = D1 {d1 :: Int}
data D2 = D2 {d2 :: Int}
data D3 = D3 {d3 :: Int}
data D4 = D4 {d4 :: Int}


data Other = Other

data Data
    = Ctor1 {field1 :: Int, field2 :: Int, field3 :: Int}
    | Ctor2 String

type Type = Data