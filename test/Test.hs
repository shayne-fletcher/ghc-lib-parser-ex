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

import Language.Haskell.GhclibParserEx.Config
import Language.Haskell.GhclibParserEx.DynFlags
import Language.Haskell.GhclibParserEx.Parse
import Language.Haskell.GhclibParserEx.Dump
import Language.Haskell.GhclibParserEx.Fixity
import Language.Haskell.GhclibParserEx.GHC.Hs.ExtendInstances
import Language.Haskell.GhclibParserEx.GHC.Hs.Expr

#if defined (GHCLIB_API_811) || defined (GHCLIB_API_810)
import GHC.Hs
import RdrHsSyn
#else
import HsSyn
#endif
import SrcLoc
import DynFlags
import Lexer
import Outputable
import ErrUtils
import GHC.LanguageExtensions.Type
import Data.List
#if defined (GHCLIB_API_808)
import Bag
#endif

main :: IO ()
main = do
  setEnv "TASTY_NUM_THREADS" "1"
  setUnsafeGlobalDynFlags (defaultDynFlags fakeSettings fakeLlvmConfig)
  defaultMain tests

tests :: TestTree
tests = testGroup " All tests"
  [ parseTests
  , fixityTests
  , extendInstancesTests
  , expressionPredicateTests
  ]

makeFile :: FilePath -> String -> IO FilePath
makeFile relPath contents = do
    Directory.createDirectoryIfMissing True $ FilePath.takeDirectory relPath
    writeFile relPath contents
    return relPath

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
  [ testCase "Module" $
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
  , testCase "Declaration (1)" $
      chkParseResult report flags $
        parseDeclaration "fact n = if n <= 1 then 1 else n * fact (n - 1)" flags
  , testCase "Declaration (2)" $ -- Example from https://github.com/ndmitchell/hlint/issues/842.
      chkParseResult report flags $
        parseDeclaration "infixr 4 <%@~" flags
  , testCase "File" $ do
      foo <- makeFile "Foo.hs" $ unlines
        ["{-# LANGUAGE ScopedTypeVariables #-}"
        , "module Foo (readMany) where"
        , "import Data.List"
        , "import Data.Maybe"
        , "readMany = unfoldr $ listToMaybe . concatMap reads . tails"
        ]
      s <- readFile' foo
      parsePragmasIntoDynFlags flags ([], []) foo s >>= \case
        Left msg -> assertFailure msg
        Right flags -> chkParseResult report flags $ parseFile foo flags s
  ]
  where
    flags = unsafeGlobalDynFlags
    report flags msgs = concat [ showSDoc flags msg | msg <- pprErrMsgBagWithLoc msgs ]

exprTest :: String -> DynFlags -> (LHsExpr GhcPs -> IO ()) -> IO ()
exprTest s flags test =
      case parseExpression s flags of
#if defined (GHCLIB_API_811) || defined (GHCLIB_API_810)
        POk s e ->
#else
        POk _ e ->
#endif
#if defined (GHCLIB_API_811) || defined (GHCLIB_API_810)
          case unP (runECP_P e >>= \e -> return e) s :: ParseResult (LHsExpr GhcPs) of
            POk _  e ->
#endif
              test e
#if defined (GHCLIB_API_811) || defined (GHCLIB_API_810)
            _ -> assertFailure "parse error"
#endif
        _ -> assertFailure "parse error"

fixityTests :: TestTree
fixityTests = testGroup "Fixity tests"
  [ testCase "Expression" $ do
      let flags = defaultDynFlags fakeSettings fakeLlvmConfig
      exprTest "1 + 2 * 3" flags
        (\e ->
            assertBool "parse tree not affected" $
              showSDocUnsafe (showAstData BlankSrcSpan e) /=
              showSDocUnsafe (showAstData BlankSrcSpan (applyFixities [] e))
        )
  , testCase "Pattern" $ do
      let flags = defaultDynFlags fakeSettings fakeLlvmConfig
      case parseDeclaration "f (1 : 2 :[]) = 1" flags of
        POk _ d ->
          assertBool "parse tree not affected" $
          showSDocUnsafe (showAstData BlankSrcSpan d) /=
          showSDocUnsafe (showAstData BlankSrcSpan (applyFixities [] d))
        PFailed{} -> assertFailure "parse error"
  ]

extendInstancesTests :: TestTree
extendInstancesTests = testGroup "Extend instances tests"
  [ testCase "Eq, Ord" $ do
      let flags = defaultDynFlags fakeSettings fakeLlvmConfig
      exprTest "1 + 2 * 3" flags
        (\e -> do
             e' <- return $ applyFixities [] e
             assertBool "astEq" $ astEq e e
             assertBool "astEq" $ not (astEq e e')
             e  <- return $ extendInstances e
             e' <- return $ extendInstances e'
             assertBool "==" $ e == e
             assertBool "/=" $ e /= e'
             assertBool "< " $ e' < e
             assertBool ">=" $ e  >= e'
          )
  ]

expressionPredicateTests :: TestTree
expressionPredicateTests = testGroup "Expression predicate tests"
  [ testCase "isTag" $ exprTest "foo" flags $ assert' . isTag "foo"
  , testCase "isTag" $ exprTest "bar" flags $ assert' . not . isTag "foo"
  , testCase "isDol" $ exprTest "f $ x" flags $ \case L _ (OpApp _ _ op _) -> assert' $ isDol op; _ -> assertFailure "unexpected"
  , testCase "isDot" $ exprTest "f . g" flags $ \case L _ (OpApp _ _ op _) -> assert' $ isDot op; _ -> assertFailure "unexpected"
  , testCase "isReturn" $ exprTest "return x" flags $ \case L _ (HsApp _ f _) -> assert' $ isReturn f; _ -> assertFailure "unexpected"
  , testCase "isReturn" $ exprTest "pure x" flags $ \case L _ (HsApp _ f _) -> assert' $ isReturn f; _ -> assertFailure "unexpected"
  , testCase "isSection" $ exprTest "(1 +)" flags $ \case L _ (HsPar _ x) -> assert' $ isSection x; _ -> assertFailure "unexpected"
  , testCase "isSection" $ exprTest "(+ 1)" flags $ \case L _ (HsPar _ x) -> assert' $ isSection x; _ -> assertFailure "unexpected"
  , testCase "isRecConstr" $ exprTest "Foo {bar=1}" flags $ assert' . isRecConstr
  , testCase "isRecUpdate" $ exprTest "foo {bar=1}" flags $ assert' . isRecUpdate
  , testCase "isVar" $ exprTest "foo" flags $ assert' . isVar
  , testCase "isVar" $ exprTest "3" flags $ assert' . not. isVar
  , testCase "isPar" $ exprTest "(foo)" flags $ assert' . isPar
  , testCase "isPar" $ exprTest "foo" flags $ assert' . not. isPar
  , testCase "isApp" $ exprTest "f x" flags $ assert' . isApp
  , testCase "isApp" $ exprTest "x" flags $ assert' . not . isApp
  , testCase "isOpApp" $ exprTest "l `op` r" flags $ assert' . isOpApp
  , testCase "isOpApp" $ exprTest "op l r" flags $ assert' . not . isOpApp
  , testCase "isAnyApp" $ exprTest "l `op` r" flags $ assert' . isAnyApp
  , testCase "isAnyApp" $ exprTest "f x" flags $ assert' . isAnyApp
  , testCase "isAnyApp" $ exprTest "f x y" flags $ assert' . isAnyApp
  , testCase "isAnyApp" $ exprTest "(f x y)" flags $ assert' . not . isAnyApp
  , testCase "isLexeme" $ exprTest "foo" flags $ assert' . isLexeme
  , testCase "isLexeme" $ exprTest "3" flags $ assert' . isLexeme
  , testCase "isLexeme" $ exprTest "f x" flags $ assert' . not . isLexeme
  , testCase "isLambda" $ exprTest "\\x -> 12" flags $ assert' . isLambda
  , testCase "isLambda" $ exprTest "foo" flags $ assert' . not . isLambda
  , testCase "isDotApp" $ exprTest "f . g" flags $ assert' . isDotApp
  , testCase "isDotApp" $ exprTest "f $ g" flags $ assert' . not . isDotApp
  , testCase "isTypeApp" $ exprTest "f @Int" flags $ assert' . isTypeApp
  , testCase "isTypeApp" $ exprTest "f" flags $ assert' . not . isTypeApp
#if defined (GHCLIB_API_808)
  , testCase "isTypeApp" $ exprTest "f @ Int" flags $ assert' . isTypeApp
#else
  , testCase "isTypeApp" $ exprTest "f @ Int" flags $ assert' . not . isTypeApp
#endif
  ]
  where
    assert' = assertBool ""
    flags = foldl' xopt_set (defaultDynFlags fakeSettings fakeLlvmConfig)
      [TypeApplications]
