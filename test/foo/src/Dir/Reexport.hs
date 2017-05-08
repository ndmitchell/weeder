
module Dir.Reexport(reexport1, reexport2, reexport3, module Dir.Everything) where

import Dir.Everything
import Dir.Orphan()

reexport1 = usedFunction2
reexport2 = show Orphan
reexport3 = ""
