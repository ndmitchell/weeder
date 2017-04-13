
module Dir.Library2(usedFunction1, reexport1, bob) where

import Dir.Reexport

bob = reexport2 `seq` usedFunction1
