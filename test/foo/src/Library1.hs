
module Library1(exported, D1(..), D2, d3) where

import Data.List.Extra

exported = chunksOf

data D1 = D1 {d1 :: Int}
data D2 = D2 {d2 :: Int}
data D3 = D3 {d3 :: Int}
data D4 = D4 {d4 :: Int}
