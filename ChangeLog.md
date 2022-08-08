# Changelog for ghc-lib-parser-ex

## 9.4.0.0  released
- Update to `ghc-lib-parser-9.4.1.20220807`

## 0.20220801 released
- Update to `ghc-lib-parser-0.20220801`

## 9.2.1.1 released
- Update to `ghc-lib-parser-9.2.4.20220527`

## 0.20220701 released
- Update to `ghc-lib-parser-0.20220701`

## 9.2.1.0 released
- The Cabal flag `auto` now defaults to `False`:
  - When `auto` has the value `False`:
    - `ghc-lib-parser-ex` depends on `ghc-lib-parser`
  - When `auto` has the value `True`:
    - When the build compiler is >=9.2.2 && <9.3.0
      - `ghc-lib-parser-ex` depends on the compiler libs
    - Otherwise, `ghc-lib-parser-ex` depends on `ghc-lib-parser`
- Deprecated modues removed:
  - `Language.Haskell.GhclibParserEx.Config`
  - `Language.Haskell.GhclibParserEx.Parse`
  - `Language.Haskell.GhclibParserEx.Outputable`

## 0.20220601 released
- Update to `ghc-lib-parser-0.20220601`
- `fakeLlvmConfig` removed for `GHCLIB_API_HEAD`

## 9.2.0.4 released
- Update to `ghc-lib-parser-9.2.3.20220527`
- Add support for ghc-9.4 series: `GHCLIB_API_904`

## 0.20220501 released
- Update to `ghc-lib-parser-0.20220501`

## 0.20220401 released
- Update to `ghc-lib-parser-0.20220401`

## 9.2.0.3 released
- Fix ghc bounds in auto mode to use ghc-lib not ghc if build compiler ghc-9.2.1 (see [issue](https://github.com/ndmitchell/hlint/issues/1314))

## 9.2.0.2 released
- Update to `ghc-lib-parser-9.2.2.20220307`

## 0.20220301 released
- Update to `ghc-lib-parser-0.20220301`

## 8.10.0.24 released
- Update to `ghc-lib-parser-8.10.7.20220219`

## 0.20220201 released
- Update to `ghc-lib-parser-0.20220201`

## 0.20220103 released
- New function `isOverLabel`
- Update to `ghc-lib-parser-0.20220103`

## 9.0.0.6 released 2021-12-26
- Add back `isQuasiQuote` for backwards compatibility

## 9.0.0.5 released 2021-12-25
- Bugfix to `isFieldPunUpdate`
- New module `Language.Haskell.GHC.Hs.Type.hs` to replace `Language.Haskell.GHC.Hs.Types.hs` (which remains for now but deprecated and will be removed in a future release)
  - New function `isKindTyApp`
- Rename `isQuasiQuote` to `isQuasiQuoteExpr`
- Add new function `isQuasiQuoteSplice`
- Update to `ghc-lib-parser-9.0.2.20211226`

## 9.2.0.1 released 2021-11-01
- Update to `ghc-lib-parser-9.2.1.20211101`

## 0.20211101 released 2021-11-01
- Update to `ghc-lib-parser-0.20211101`

## 9.2.0.0 released 2021-10-30
- Update to `ghc-lib-parser-9.2.1.20211030`

## 0.20211001 released 2021-10-01
- Add `isSplicePat` to `Language.Haskell.GhclibParserEx.GHC.Hs.Pat`
- Use `genericPlatform` on `GHCLIB_API_HEAD` in `GhclibParserEx.GHC.Settings.Config.hs`
- Update to `ghc-lib-parser-0.20211001`

## 0.20210901 released 2021-09-01
- Update to `ghc-lib-parser-0.20210901`

## 8.10.0.23 released 2021-08-28
- Update to `ghc-lib-parser-8.10.7.20210828`

## 8.10.0.22 released 2021-08-14
- Update to `ghc-lib-parser-0.20210814`
- Added to `GhclibParserEx.GHC.Hs.Expr`:
  - `isMonadComp`
  - `isListComp`

## 0.20210701 released 2021-07-01
- Update to `ghc-lib-0.20210701`

## 8.10.0.21 released 2021-06-07
- Bugfix cabal files

## 8.10.0.20 released 2021-06-06
- Update to `ghc-lib-8.10.5.20210606`

## 0.20210601 released 2021-06-01
- Update to `ghc-lib-parser-0.20210601`
- Update types in `GHC.Types.Name.Reader` for ghc-9.2.1, `Located` becomes `LocatedN`

## 0.20210501 released 2021-05-01
- Update to `ghc-lib-0.20210501`

## 0.20210331 released 2021-02-31
- Update to `ghc-lib-0.20210331`
- Update to `ghc-lib-parser-9.0.1.20210324`

## 9.0.0.4 released 2021-03-11
- Bugfix for `GHC.Hs.Dump`

## 0.20210228 released 2021-02-28
- Update to `ghc-lib-0.20210228`

## 9.0.0.3 released 2021-02-08
- Cabal bugfix

## 9.0.0.2 released 2021-02-08
- Cabal bugfix

## 8.10.0.19 released 2021-02-08
- Cabal bugfix

## 8.10.0.18 released 2021-02-06
- Update to ghc-8.10.4.

## 9.0.0.1 released 2021-02-05
- Upgrade Cabal defaults flag to 9.0.1

## 9.0.0.0 released 2021-02-05
- Update to ghc-9.0.1

## 0.20210201 released 2021-02-01
- Update to `ghc-lib-0.20210201`

## 0.20210101 released 2021-01-01
- Update to `ghc-lib-0.20210101`

## 8.10.0.17 released 2020-12-20
- Update to ghc-8.10.3.

## 0.20201101 released 2020-11-01
- Update to `ghc-lib-0.20201101`

## 0.20201001 released 2020-10-01
- Update to `ghc-lib-0.20201001`
- `GHCLIB_API_811` -> `GHCLIB_API_HEAD`
- Add support for `GHCLIB_API_900`

## 0.20200901 released 2020-09-01
- Update to `ghc-lib-0.20200901`

## 8.10.0.16 released 2020-08-08
- Update to ghc-8.10.2.

## 0.20200801 released 2020-08-01
- Update to `ghc-lib-0.20200801`

## 8.8.6.1 released 2020-07-16

## 0.20200704 released 2020-07-04
- New function `isImportQualifiedPost`

## 8.10.0.15 released 2020-07-04
- New function `isImportQualifiedPost`

## 8.10.0.14 released 2020-06-10
- New function `isSymbolRdrName`
- New module
  - `Language.Haskell.GhclibParserEx.GHC.Settings.Config` to replace `Language.Haskell.GhclibParserEx.Config` (which remains for now but deprecated and will be removed in a future release)

## 0.20200601 released 2020-06-01

## 8.10.0.13 released 2020-05-31
- Sync `extra` with HLint

## 8.10.0.12 released 2020-05-31
- New module `Language.Haskell.GhclibParserEx.GHC.Hs`

## 8.10.0.11 released 2020-05-18
- Upgrade to `ghc-lib-parser-8.10.1.20200523`

## 8.10.0.10 released 2020-05-18
- Upgrade to `ghc-lib-parser-8.10.1.20200518`

## 8.10.0.9 released 2020-05-16
- New modules
  - `Language.Haskell.GhclibParserEx.GHC.Hs.Binds`
  - `Language.Haskell.GhclibParserEx.GHC.Hs.ImpExp`

## 8.10.0.8 released 2020-05-14
- New module `Language.Haskell.GhclibParserEx.GHC.Types.Name.Reader`

## 8.10.0.7 released 2020-05-13
- New function `fixitiesFromModule`

## 8.10.0.6 released 2020-05-05
- Bugfix in `parsePragmasIntoDynFlags` that meant that default enabled/disabled extensions subsequently disabled/enabled via pragma weren't getting disabled/enabled

## 8.10.0.5 released 2020-05-02
- New modules
  - `Language.Haskell.GhclibParserEx.GHC.Parser`, `Language.Haskell.GhcLibParserEx.GHC.Utils.Outputable` to replace `Language.Haskell.GhclibParserEx.Parse` and `Language.Haskell.GhclibParserEx.Outputable` (which remain for now but deprecated and will be removed in a future release)

## 0.20200501 released 2020-05-01

## 8.10.0.4 released 2020-04-04
- Add expression predicates `isWholeFrac`, `isFieldPunUpdate`, `isStrictMatch`, `isMultiIf`, `isProc`, `isTransStmt`;
- Add pattern predicate `isPFieldPun`.

## 8.10.0.3 released 2020-04-03
- `strToPat` now returns an `LPat GhcPs`
- `parseExpression` now returns an `ParseResult (LHsExpr GhcPs)` (>= ghc-8.10)

## 0.20200401 released 2020-04-01

## 8.10.0.2 released 2020-03-30
- Rework cabal flags; allow full configurability with a good default:
  - Have two flags `auto` and `no-ghc-lib`. Default behavior exactly as `hlint` linking `ghc-lib-parser-8.10.*` if not on `ghc-8.10.*` and `ghc-8.10.*` otherwise.

## 8.10.0.1 released 2020-03-28
- Unless the Cabal flag `ghc-lib` is `true` link native ghc-libs (without regard for the compiler version)
- Change the signature of `hasPFieldsDotDot`
  - This has no impact on 8.8 parse trees but matters when it comes to >= 8.10
- Change the signature of `isPFieldWildcard`
  - This has no impact on 8.8 parse trees but matters when it comes to >= 8.10

## 8.10.0.0 released 2020-03-24
- First release of the ghc-8.10 series

## 8.8.6.0 released 2020-03-22
- `Language.Haskell.GhclibParserEx.DynFlags` is now `Language.Haskell.GhclibParserEx.GHC.Driver.Session`

## 8.8.5.8 released 2020-03-17
- New module `Language.Haskell.GhclibParserEx.GHC.Driver.Flags`
  - Export `Bounded` instance for `Language`
    (https://github.com/shayne-fletcher/ghc-lib-parser-ex/issues/30)

## 8.8.5.7 released 2020-03-16
- From `Language.Haskell.GhclibParserEx.Fixity`:
  - Supply missing fixities (https://github.com/ndmitchell/hlint/issues/913)
- In `Language.Haskell.GhclibParserEx.DynFlags`:
  - Give `Extension` an `Ord` instance

## 8.8.5.6 released 2020-03-13
- From `Language.Haskell.GhclibParserEx.Fixity`:
  - Expose `infixr_`, `infixl_`, `infix_` and `fixity`

## 8.8.5.5 released 2020-03-12
- Remove from `Language.Haskell.GhclibParserEx.Fixity`:
   - `preludeFixities`
   - `baseFixities`

## 8.8.5.4 released 2020-03-11
- Expose from `Language.Haskell.GhclibParserEx.Fixity`:
   - `preludeFixities`
   - `baseFixities`
   - `lensFixities`
   - `otherFixities`

## 0.20200301 released 2020-03-01

## 8.8.5.3 released 2020-02-25
- New modules:
  - `Language.Haskell.GhclibParserEx.Pat`
  - `Language.Haskell.GhclibParserEx.Types`
  - `Language.Haskell.GhclibParserEx.Decls`

## 8.8.5.2 released 2020-02-16
- New `DynFlags` functions `readExtension`, `extensionImplications`.

## 8.8.5.1 released 2020-02-09
- Expression predicate tests.

## 8.8.5.0 released 2020-02-07
- Expose `impliedGFlags` and friends from `DynFlags` (https://github.com/shayne-fletcher/ghc-lib-parser-ex/issues/19).

## 8.8.4.0 released 2020-02-01
- New modules:
  - `Language.Haskell.GhclibparserEx.GHC.Hs.Expr`
- Moved modules:
  - `Language.Haskell.GhclibparserEx.HsExtendInstances` ->
    `Language.Haskell.GhclibparserEx.GHC.Hs.ExtendInstances`;

## 0.20200201.1.0 released 2020-02-01
- New modules:
  - `Language.Haskell.GhclibparserEx.HsExtendInstances`.

## 8.8.3.0 released 2020-01-25
- Change in versioning scheme;
- New modules:
  - `Language.Haskell.GhclibParserEx.Config`
  - `Language.Haskell.GhclibParserEx.DynFlags`
- `parsePragmasIntoDynFlags` signature change.

## 8.8.1.20191204, 8.8.2, 0.20200102 released 2020-01-18
- First releases
