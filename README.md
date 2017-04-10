# Weeder [![Hackage version](https://img.shields.io/hackage/v/weeder.svg?label=Hackage)](https://hackage.haskell.org/package/weeder) [![Stackage version](https://www.stackage.org/package/weeder/badge/lts?label=Stackage)](https://www.stackage.org/package/weeder) [![Linux Build Status](https://img.shields.io/travis/ndmitchell/weeder.svg?label=Linux%20build)](https://travis-ci.org/ndmitchell/weeder) [![Windows Build Status](https://img.shields.io/appveyor/ci/ndmitchell/weeder.svg?label=Windows%20build)](https://ci.appveyor.com/project/ndmitchell/weeder)

The principle is to delete dead code. To do that, run:

* GHC with `-fwarn-unused-binds -fwarn-unused-imports`
* HLint, looking for "Redundant extension" hints
* This tool, which mops up the rest - detecting dead exports or package imports
