# Weeder [![Hackage version](https://img.shields.io/hackage/v/weeder.svg?label=Hackage)](https://hackage.haskell.org/package/weeder) [![Stackage version](https://www.stackage.org/package/weeder/badge/lts?label=Stackage)](https://www.stackage.org/package/weeder) [![Linux Build Status](https://img.shields.io/travis/ndmitchell/weeder.svg?label=Linux%20build)](https://travis-ci.org/ndmitchell/weeder) [![Windows Build Status](https://img.shields.io/appveyor/ci/ndmitchell/weeder.svg?label=Windows%20build)](https://ci.appveyor.com/project/ndmitchell/weeder)

The principle is to delete dead code (pulling up the weeds). To do that, run:

* GHC with `-fwarn-unused-binds -fwarn-unused-imports`, which finds unused definitions and unused imports.
* [HLint](https://github.com/ndmitchell/hlint#readme), looking for "Redundant extension" hints, which finds unused extensions.
* This tool, `weeder .` which detects redundant `build-depends` in the `.cabal` and functions that are exported internally but not available outside this library.

Caveats:

* For `build-depends`, the list of unused packages should be accurate. It will not report packages that are unused but declared to only be required in certain configurations. It will not report packages that are declared used but only used via transitive dependencies of used dependencies.
* For unused exported things, it may be incorrect if:
  * The type is exported, not used, but has functions which have that type. (Technically this is unused, but I wouldn't encourage deleting such types.)
  * It is only used in modules from which it is reexported.
  * It is used in some modules which aren't exposed.
