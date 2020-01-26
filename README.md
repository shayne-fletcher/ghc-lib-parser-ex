# ghc-lib-parser-ex [![Build Status](https://shayne-fletcher.visualstudio.com/ghc-lib-parser-ex/_apis/build/status/shayne-fletcher.ghc-lib-parser-ex?branchName=master)](https://shayne-fletcher.visualstudio.com/ghc-lib-parser-ex/_build/latest?definitionId=1&branchName=master)
Copyright © 2020, Shayne Fletcher. All rights reserved.
SPDX-License-Identifier: BSD-3-Clause

The `ghc-lib-parser-ex` package contains GHC API parse tree utilities. It works with or without [`ghc-lib-parser`](https://github.com/digital-asset/ghc-lib).

## Using `ghc-lib-parser-ex`

Package `ghc-lib-parser-ex` is on [Hackage](https://hackage.haskell.org/package/ghc-lib-parser-ex) e.g. `cabal install ghc-lib-parser-ex`. Like `ghc-lib-parser`, there are two release streams within the `ghc-lib-parser-ex` name.

Version numbers are of the form α.β.γ.δ where α.β corresponds to a GHC series and γ.δ are the major and minor parts of the `ghc-lib-ex-parser` package release. Examples:
* Version 8.10.1.3 is compatible with any `ghc-lib-parser-8.10.*` (or `ghc-8.10.*`) package;
* Version 0.20190204.2.0 is compatible with [`ghc-lib-parser-0.20190204`](http://hackage.haskell.org/package/ghc-lib-0.20190204).

The major part γ of the release number indicates an interface breaking change from the previous release. The minor part δ indicates a non-interface breaking change from the previous release.

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

Update the [changelog](./ChangeLog.md), `git tag <version> && git push origin <version>` then build via the [above instructions](#building-ghc-lib-parser-ex) and upload the resulting `.tar.gz` files to [Hackage](https://hackage.haskell.org/upload).
