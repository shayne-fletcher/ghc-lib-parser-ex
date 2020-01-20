-- Copyright (c) 2020, Shayne Fletcher. All rights reserved.
-- SPDX-License-Identifier: BSD-3-Clause.

{-# LANGUAGE CPP #-}
{-# LANGUAGE LambdaCase #-}
#include "ghclib_api.h"

import Test.Tasty
import Test.Tasty.HUnit
import System.Directory as Directory
import System.Environment
import qualified System.FilePath as FilePath
import System.IO.Extra
import Control.Monad

import Language.Haskell.GhclibParserEx.Parse
import Language.Haskell.GhclibParserEx.Dump
import Language.Haskell.GhclibParserEx.Fixity

#if defined (GHCLIB_API_811) || defined (GHCLIB_API_810)
import GHC.Hs
import RdrHsSyn
#endif
import DynFlags
import Lexer
import Outputable
import ErrUtils
#if defined (GHCLIB_API_808)
import Bag
#endif

main :: IO ()
main = do
  setEnv "TASTY_NUM_THREADS" "1"
  setUnsafeGlobalDynFlags (defaultDynFlags fakeSettings fakeLlvmConfig)
  defaultMain tests

tests :: TestTree
tests = testGroup " All tests" [parseTests, fixityTests]

chkParseResult :: (DynFlags -> WarningMessages -> String) -> DynFlags -> ParseResult a -> IO ()
chkParseResult report flags = \case
    POk s _ -> do
      let (wrns, errs) = getMessages s flags
      when (not (null errs) || not (null wrns)) $
        assertFailure (report flags wrns ++ report flags errs)
#if defined (GHCLIB_API_811) || defined (GHCLIB_API_810)
    PFailed s -> assertFailure (report flags $ snd (getMessages s flags))
#else
    PFailed _ loc err -> assertFailure (report flags $ unitBag $ mkPlainErrMsg flags loc err)
#endif

parseTests :: TestTree
parseTests = testGroup "Parse tests"
  [
    testCase "Module" $
      chkParseResult report flags $
        parseModule (unlines
          [ "module Foo (readMany) where"
          , "import Data.List"
          , "import Data.Maybe"
          , "readMany = unfoldr $ listToMaybe . concatMap reads . tails"
          ]) flags
  , testCase "Signature" $
      chkParseResult report flags $
        parseSignature (unlines
          [ "signature Str where"
          , "data Str"
          , "empty :: Str"
          , "append :: Str -> Str -> Str"
          ]) flags
  , testCase "Import" $
      chkParseResult report flags $
        parseImport "import qualified \"foo-lib\" Foo as Bar hiding ((<.>))" flags
  , testCase "Statement" $
      chkParseResult report flags $
        parseStatement "Foo foo <- bar" flags
  , testCase "Backpack" $
      chkParseResult report flags $
        parseBackpack (unlines
          [ "unit main where"
          , "  module Main where"
          , "    main = putStrLn \"Hello world!\""
          ]) flags
  , testCase "Expression" $
      chkParseResult report flags $
        parseExpression "unfoldr $ listToMaybe . concatMap reads . tails" flags
  , testCase "Declaration" $
      chkParseResult report flags $
        parseDeclaration "fact n = if n <= 1 then 1 else n * fact (n - 1)" flags
  , testCase "File" $ do
      foo <- makeFile "Foo.hs" $ unlines
        ["{-# LANGUAGE ScopedTypeVariables #-}"
        , "module Foo (readMany) where"
        , "import Data.List"
        , "import Data.Maybe"
        , "readMany = unfoldr $ listToMaybe . concatMap reads . tails"
        ]
      s <- readFile' foo
      parsePragmasIntoDynFlags flags foo s >>= \case
        Left msg -> assertFailure msg
        Right flags -> chkParseResult report flags $ parseFile foo flags s
  ]
  where
    flags = unsafeGlobalDynFlags
    report flags msgs = concat [ showSDoc flags msg | msg <- pprErrMsgBagWithLoc msgs ]

fixityTests :: TestTree
fixityTests = testGroup "Fixity tests"
  [
    testCase "Expression" $ do
      let flags = defaultDynFlags fakeSettings fakeLlvmConfig
      case parseExpression "1 + 2 * 3" flags of
#if defined (GHCLIB_API_811) || defined (GHCLIB_API_810)
        POk s e ->
#else
        POk _ e ->
#endif
#if defined (GHCLIB_API_811) || defined (GHCLIB_API_810)
          case unP (runECP_P e >>= \e -> return e) s :: ParseResult (LHsExpr GhcPs) of
            POk _  e ->
#endif
              assertBool "parse tree not affected" $
                showSDocUnsafe (showAstData BlankSrcSpan e) /=
                showSDocUnsafe (showAstData BlankSrcSpan (applyFixities [] e))
#if defined (GHCLIB_API_811) || defined (GHCLIB_API_810)
            PFailed{} -> assertFailure "ecp failure"
#endif
        PFailed{} -> assertFailure "parse error"
  , testCase "Pattern" $ do
      let flags = defaultDynFlags fakeSettings fakeLlvmConfig
      case parseDeclaration "f (1 : 2 :[]) = 1" flags of
        POk _ d ->
          assertBool "parse tree not affected" $
          showSDocUnsafe (showAstData BlankSrcSpan d) /=
          showSDocUnsafe (showAstData BlankSrcSpan (applyFixities [] d))
        PFailed{} -> assertFailure "parse error"
  ]

makeFile :: FilePath -> String -> IO FilePath
makeFile relPath contents = do
    Directory.createDirectoryIfMissing True $ FilePath.takeDirectory relPath
    writeFile relPath contents
    return relPath
