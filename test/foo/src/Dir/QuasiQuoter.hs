{-# OPTIONS_GHC -Wno-missing-fields #-}

module Dir.QuasiQuoter(quasi) where

import Language.Haskell.TH.Quote

{-# NOINLINE quasi #-}
quasi :: QuasiQuoter
quasi = QuasiQuoter {quoteDec = const $ return []}
