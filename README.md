# ghc-lib-parser-ex [![Build Status](https://shayne-fletcher.visualstudio.com/ghc-lib-parser-ex/_apis/build/status/shayne-fletcher.ghc-lib-parser-ex?branchName=master)](https://shayne-fletcher.visualstudio.com/ghc-lib-parser-ex/_build/latest?definitionId=1&branchName=master)
Copyright © 2020, Shayne Fletcher. All rights reserved.
SPDX-License-Identifier: BSD-3-Clause

The `ghc-lib-parser-ex` package contains GHC API parse tree utilities. It works with or without [`ghc-lib-parser`](https://github.com/digital-asset/ghc-lib).

## Using `ghc-lib-parser-ex`

Package `ghc-lib-parser-ex` is on [Hackage](https://hackage.haskell.org/package/ghc-lib-parser-ex) e.g. `cabal install ghc-lib-parser-ex`. There are two release streams within the `ghc-lib-parser-ex` name (tracking `ghc-lib-parser`):

* Version 8.10.1 will be `ghc-lib-parser-ex` for use with `ghc-lib-parser-8.10.1`(or the released GHC 8.10.1);
* Version 0.20190204 is for use with [`ghc-lib-parser-0.20190204`](http://hackage.haskell.org/package/ghc-lib-0.20190204).

## Building `ghc-lib-parser-ex`

You can build with `stack build` and test with `stack test`. Produce `ghc-lib-parser-ex` package distributions by executing the CI script:
```bash
# Setup
git clone git@github.com:shayne-fletcher/ghc-lib-parser-ex.git
cd ghc-lib-parser-ex
stack runhaskell --package extra --package optparse-applicative CI.hs
```
Run `stack runhaskell --package extra --package optparse-applicative CI.hs -- --help` for more options.

To run [`hlint`](https://github.com/ndmitchell/hlint) on this repository, `hlint --cpp-include cbits --cpp-define GHCLIB_API_XXX .` (where `XXX` at this time is one of `808`, `810` or `811`).

## Releasing `ghc-lib-parser-ex` (notes for maintainers)

Build `ghc-lib-parser-ex` using the [above instructions](#building-ghc-lib-parser-ex)  and upload the resulting `.tar.gz` files to [Hackage](https://hackage.haskell.org/upload).
