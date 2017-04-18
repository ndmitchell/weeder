
module Dir.Library2(usedFunction1, reexport1, bob, MyClass3(..), myClass1, foo, (=~=)) where

import Dir.Reexport

bob = reexport2 `seq` usedFunction1

class MyClass1 a where myClass1 :: a
class MyClass2 a where myClass2 :: a
class MyClass3 a where myClass3 :: a

foo _ = myClass2
