-- Copyright (c) 2020, Shayne Fletcher. All rights reserved.
-- SPDX-License-Identifier: BSD-3-Clause.

{-# LANGUAGE CPP #-}
{-# LANGUAGE LambdaCase #-}
#include "ghclib_api.h"

import Test.Tasty
import Test.Tasty.HUnit
import System.Directory as Directory
import qualified System.FilePath as FilePath
import System.IO.Extra
import Control.Monad

import Language.Haskell.GhclibParserEx.Parse
import DynFlags
import Lexer
import Outputable
import ErrUtils
#if defined (GHCLIB_API_808)
import Bag
#endif

main = defaultMain tests

tests :: TestTree
tests = testGroup " All tests" [parseTests]

chkParseResult report flags = \case
    POk s m -> do
      let (wrns, errs) = getMessages s flags
      when (not (null errs) || not (null wrns)) $
        assertFailure (report flags wrns ++ report flags errs)
#if defined (GHCLIB_API_811) || defined (GHCLIB_API_810)
    PFailed s -> assertFailure (report flags $ snd (getMessages s flags))
#else
    PFailed _ loc err -> assertFailure (report flags $ unitBag $ mkPlainErrMsg flags loc err)
#endif

parseTests = testGroup "Parse tests"
  [
    testCase "Parse expression" $ do
      let flags = defaultDynFlags fakeSettings fakeLlvmConfig
      chkParseResult report flags $
        parseExpr "unfoldr $ listToMaybe . concatMap reads . tails" flags
  , testCase "Parse import" $ do
      let flags = defaultDynFlags fakeSettings fakeLlvmConfig
      chkParseResult report flags $
        parseImport "import qualified \"foo-lib\" Foo as Bar hiding ((<.>))" flags
  , testCase "Parse trivial module" $ do
      foo <- makeFile "Foo.hs" $ unlines
        [ "module Foo (readMany) where"
        , "import Data.List"
        , "import Data.Maybe"
        , "readMany = unfoldr $ listToMaybe . concatMap reads . tails"
        ]
      s <- readFile' foo
      parsePragmasIntoDynFlags (defaultDynFlags fakeSettings fakeLlvmConfig) foo s >>= \case
        Left msg -> assertFailure msg
        Right flags ->
          chkParseResult report flags $ parseFile foo (flags `gopt_set` Opt_KeepRawTokenStream) s
  ]
  where
    report flags msgs = concat [ showSDoc flags msg | msg <- pprErrMsgBagWithLoc msgs ]

makeFile :: FilePath -> String -> IO FilePath
makeFile relPath contents = do
    Directory.createDirectoryIfMissing True $ FilePath.takeDirectory relPath
    writeFile relPath contents
    return relPath
