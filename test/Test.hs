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

parseExpressionTest :: String -> DynFlags -> (LHsExpr GhcPs -> IO ()) -> IO ()
parseExpressionTest s flags test =
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
  [
    testCase "Expression" $ do
      let flags = defaultDynFlags fakeSettings fakeLlvmConfig
      parseExpressionTest "1 + 2 * 3" flags
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
  [
    testCase "Eq, Ord" $ do
      let flags = defaultDynFlags fakeSettings fakeLlvmConfig
      parseExpressionTest "1 + 2 * 3" flags
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
  [
    testCase "isTag" $ parseExpressionTest "foo" flags $ assertBool "isTag" . isTag "foo"
  , testCase "isDol" $ parseExpressionTest "f $ x" flags
    (\e -> case e of
        L _ (OpApp _ _ op _) -> assertBool "" $ isDol op
        _ -> assertFailure "unexpected" )
  , testCase "isDot" $ parseExpressionTest "f . g" flags
    (\e -> case e of
         L _ (OpApp _ _ op _) -> assertBool "" $ isDot op
         _ -> assertFailure "unexpected" )
  , testCase "isReturn" $ parseExpressionTest "return x" flags
    (\e -> case e of
         L _ (HsApp _ f _) -> assertBool "" $ isReturn f
         _ -> assertFailure "unexpected" )
  , testCase "isReturn" $ parseExpressionTest "pure x" flags
    (\e -> case e of
         L _ (HsApp _ f _) -> assertBool "" $ isReturn f
         _ -> assertFailure "unexpected" )
  , testCase "isSection" $ parseExpressionTest "(1 +)" flags
    (\e -> case e of
         L _ (HsPar _ x) -> assertBool "" $ isSection x
         _ -> assertFailure "unexpected" )
  , testCase "isSection" $ parseExpressionTest "(+ 1)" flags
    (\e -> case e of
         L _ (HsPar _ x) -> assertBool "" $ isSection x
         _ -> assertFailure "unexpected" )
  , testCase "isRecConstr" $ parseExpressionTest "Foo {bar=1}" flags $ assertBool "" . isRecConstr
  , testCase "isRecUpdate" $ parseExpressionTest "foo{bar=1}" flags $ assertBool "" . isRecUpdate
  , testCase "isVar" $ parseExpressionTest "foo" flags $ assertBool "" . isVar
  , testCase "isPar" $ parseExpressionTest "(foo)" flags $ assertBool "" . isPar
  , testCase "isApp" $ parseExpressionTest "f x" flags $ assertBool "" . isApp
  , testCase "isOpApp" $ parseExpressionTest "l `op` r" flags $ assertBool "" . isOpApp
  , testCase "isAnyApp" $ parseExpressionTest "l `op` r" flags $ assertBool "" . isAnyApp
  , testCase "isAnyApp" $ parseExpressionTest "f x" flags $ assertBool "" . isAnyApp
  , testCase "isLexeme" $ parseExpressionTest "foo" flags $ assertBool "" . isLexeme
  , testCase "isLexeme" $ parseExpressionTest "3" flags $ assertBool "" . isLexeme
  , testCase "isLambda" $ parseExpressionTest "\\_ -> 12" flags $ assertBool "" . isLambda
  , testCase "isDotApp" $ parseExpressionTest "f . g" flags $ assertBool "" . isDotApp
  , testCase "isTypeApp" $ parseExpressionTest "f @ Int" flags $ assertBool "" . isTypeApp
  ]
  where
    flags = foldl' xopt_set (defaultDynFlags fakeSettings fakeLlvmConfig)
      [TypeApplications]
