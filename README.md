# Weeder [![Hackage version](https://img.shields.io/hackage/v/weeder.svg?label=Hackage)](https://hackage.haskell.org/package/weeder) [![Stackage version](https://www.stackage.org/package/weeder/badge/lts?label=Stackage)](https://www.stackage.org/package/weeder) [![Linux Build Status](https://img.shields.io/travis/ndmitchell/weeder.svg?label=Linux%20build)](https://travis-ci.org/ndmitchell/weeder) [![Windows Build Status](https://img.shields.io/appveyor/ci/ndmitchell/weeder.svg?label=Windows%20build)](https://ci.appveyor.com/project/ndmitchell/weeder)

The principle is to delete dead code (pulling up the weeds). To do that, run:

* GHC with `-fwarn-unused-binds -fwarn-unused-imports`, which finds unused definitions and unused imports.
* [HLint](https://github.com/ndmitchell/hlint#readme), looking for "Redundant extension" hints, which finds unused extensions.
* This tool, `weeder .` which detects redundant `build-depends` in the `.cabal` and functions that are exported internally but not available outside this library.

To use `weeder` your code must be building with `stack`, as it piggy-backs off some files `stack` generates. If you don't normally build with `stack` a simple `stack init && weeder . --build` is likely to be sufficient.
