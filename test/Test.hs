-- Copyright (c) 2020, Shayne Fletcher. All rights reserved.
-- SPDX-License-Identifier: BSD-3-Clause.

{-# LANGUAGE CPP #-}
#include "ghclib_api.h"

import Test.Tasty
import Test.Tasty.HUnit
import System.Directory as Directory
import qualified System.FilePath as FilePath
import System.IO.Extra
import Control.Monad

import Language.Haskell.GhclibParserEx.Parser
import DynFlags
import Lexer
import Outputable
import ErrUtils
#if defined (GHCLIB_API_808)
import Bag
#endif

main = defaultMain tests

tests :: TestTree
tests = testGroup " All tests" [parserTests]

parserTests = testGroup "Parser tests"
  [ testCase "Parse trivial module" $ do
      foo <- makeFile "Foo.hs" $ unlines
        [ "module Foo where"
        , "foo = putStrLn \"Hello world!\""
        ]
      s <- readFile' foo
      flags <- parsePragmasIntoDynFlags (defaultDynFlags fakeSettings fakeLlvmConfig) foo s
      case flags of
        Left msg -> assertFailure msg
        Right flags ->
          case parseFile foo (flags `gopt_set` Opt_KeepRawTokenStream)s of
#if defined (GHCLIB_API_811) || defined (GHCLIB_API_810)
              PFailed s ->
                assertFailure (report flags $ snd (getMessages s flags))
#else
              PFailed _ loc err ->
                assertFailure (report flags $ unitBag $ mkPlainErrMsg flags loc err)
#endif
              POk s m -> do
                let (wrns, errs) = getMessages s flags
                when (not (null errs) || not (null wrns)) $
                 assertFailure (report flags wrns ++ report flags errs)
      return ()
  ]
  where
    report flags msgs = concat [ showSDoc flags msg | msg <- pprErrMsgBagWithLoc msgs ]

makeFile :: FilePath -> String -> IO FilePath
makeFile relPath contents = do
    Directory.createDirectoryIfMissing True $ FilePath.takeDirectory relPath
    writeFile relPath contents
    return relPath
