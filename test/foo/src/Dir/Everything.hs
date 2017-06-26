{-# LANGUAGE TemplateHaskell #-}

module Dir.Everything(module Data.List.Extra, module Dir.Everything) where

import Data.List.Extra
import Language.Haskell.TH.Syntax
import System.Timeout

usedFunction1 = undefined :: (T1 -> T1) -> ()
usedFunction2 = Other
unusedFunction = 12 :: Int
(=~=) a b = a == b -- used
(==~==) a b = a == b

class ClassWithFunc a where
    classWithFunc :: a

data D1 = D1 {d1 :: Int}
data D2 = D2 {d2 :: Int}
data D3 = D3 {d3 :: Int}
data D4 = D4 {d4 :: Int}
data D5 = D5_ {d5 :: Int}
data D6 = D6_ Int
data D7 = D7 Int

type T1 = D1
type T2 = D2

data Other = Other

data Data
    = Ctor1 {field1 :: Int, field2 :: Int, field3 :: Int}
    | Ctor2 String

type Type = Data

data Orphan = Orphan

templateHaskell :: Int
templateHaskell = $(timeout `seq` lift (1 :: Int))
