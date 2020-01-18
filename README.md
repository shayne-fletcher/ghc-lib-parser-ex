# ghc-lib-parser-ex [![Build Status](https://shayne-fletcher.visualstudio.com/ghc-lib-parser-ex/_apis/build/status/shayne-fletcher.ghc-lib-parser-ex?branchName=master)](https://shayne-fletcher.visualstudio.com/ghc-lib-parser-ex/_build/latest?definitionId=1&branchName=master)
Copyright © 2020, Shayne Fletcher. All rights reserved.
SPDX-License-Identifier: BSD-3-Clause

The `ghc-lib-parser-ex` package contains GHC API parse tree utilities. It works with or without [`ghc-lib-parser`](https://github.com/digital-asset/ghc-lib).

## Using `ghc-lib-parser-ex`

The package `ghc-lib-parser-ex` is available on [Hackage](https://hackage.haskell.org/) e.g. `cabal install ghc-lib-parser-ex`. There are two release streams within the `ghc-lib-parser-ex` name (tracking released `ghc-lib-parser` versions exactly):

* Version 8.10.1 will be the `ghc-lib-parser-ex` for use against `ghc-lib-parser-8.10.1`(or the released GHC 8.10.1);
* Version 0.20190204 is for use against [`ghc-lib-parser-0.20190204`](http://hackage.haskell.org/package/ghc-lib-0.20190204).

### Building `ghc-lib-parser-ex`

You can build the library with `stack build` and test it with `stack test`. You can also produce the `ghc-lib-parser-ex` package by executing the CI script which incidentally builds and runs the tests.
```bash
# Setup
git clone git@github.com:shayne-fletcher/ghc-lib-parser-ex.git
cd ghc-lib-parser-ex
stack runhaskell --package extra --package optparse-applicative CI.hs
```
Run `stack runhaskell --package extra --package optparse-applicative CI.hs -- --help` for more configurability options.

## Releasing `ghc-lib` (notes for maintainers)

Build `ghc-lib-parser-ex` using the [above instructions](#building-ghc-lib-parser-ex)  and upload the resulting `.tar.gz` files to [Hackage](https://hackage.haskell.org/upload).
