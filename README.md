# weeder

The principle is to delete dead code. To do that, run:

* GHC with `-fwarn-unused-binds -fwarn-unused-imports`
* HLint, looking for "Redundant extension" hints
* This tool, which mops up the rest - detecting dead exports or package imports
