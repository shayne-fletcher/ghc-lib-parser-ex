# Changelog for ghc-lib-parser-ex

## Unreleased changes

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
