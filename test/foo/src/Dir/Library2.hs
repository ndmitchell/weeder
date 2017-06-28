{-# LANGUAGE QuasiQuotes #-}

module Dir.Library2(usedFunction1, reexport1, bob, MyClass3(..), myClass1, foo, (=~=), coerced) where

import Dir.Reexport
import Dir.QuasiQuoter
import Data.Coerce
import Dir.CoerceType
import Dir.CoerceValue

bob = reexport2 `seq` usedFunction1

class MyClass1 a where myClass1 :: a
class MyClass2 a where myClass2 :: a
class MyClass3 a where myClass3 :: a

foo _ = (myClass2, classWithFunc)

[quasi| hello |]

coerced = coerce coerceValue :: Int
