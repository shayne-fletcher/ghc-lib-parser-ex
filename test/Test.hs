-- Copyright (c) 2020, Shayne Fletcher. All rights reserved.
-- SPDX-License-Identifier: BSD-3-Clause.

{-# LANGUAGE CPP #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeApplications #-}
#include "ghclib_api.h"

import Test.Tasty
import Test.Tasty.HUnit
import System.FilePath
import System.Directory as Directory
import System.Environment
import qualified System.FilePath as FilePath
import System.IO.Extra
import Control.Monad
import Data.List.Extra
import Data.Maybe
import Data.Generics.Uniplate.Data
#if defined (GHCLIB_API_HEAD) || defined (GHCLIB_API_904) || defined (GHCLIB_API_902)
import GHC.Data.Bag
#if !defined (GHCLIB_API_902)
import GHC.Driver.Errors.Types
#endif
import GHC.Types.Error
#endif

import Language.Haskell.GhclibParserEx.Dump
import Language.Haskell.GhclibParserEx.Fixity
import Language.Haskell.GhclibParserEx.GHC.Settings.Config
import Language.Haskell.GhclibParserEx.GHC.Parser
import Language.Haskell.GhclibParserEx.GHC.Hs
import Language.Haskell.GhclibParserEx.GHC.Hs.ExtendInstances
import Language.Haskell.GhclibParserEx.GHC.Hs.Expr
import Language.Haskell.GhclibParserEx.GHC.Hs.Pat
import Language.Haskell.GhclibParserEx.GHC.Hs.Type
-- We only test 'isImportQualifiedPost' at this time which requires >=
-- 8.10; avoid unused import warning.
#if defined (MIN_VERSION_ghc_lib_parser)
#  if !MIN_VERSION_ghc_lib_parser(1,  0,  0) || MIN_VERSION_ghc_lib_parser(8, 10, 0)
import Language.Haskell.GhclibParserEx.GHC.Hs.ImpExp
#  endif
#elif __GLASGOW_HASKELL__ >= 810
import Language.Haskell.GhclibParserEx.GHC.Hs.ImpExp
#endif
import Language.Haskell.GhclibParserEx.GHC.Driver.Flags()
import Language.Haskell.GhclibParserEx.GHC.Driver.Session
import Language.Haskell.GhclibParserEx.GHC.Types.Name.Reader

#if defined (GHCLIB_API_HEAD) || defined (GHCLIB_API_904) || defined (GHCLIB_API_902) || defined (GHCLIB_API_900) || defined (GHCLIB_API_810)
import GHC.Hs
#else
import HsSyn
#endif
#if defined (GHCLIB_API_HEAD) || defined (GHCLIB_API_904) || defined (GHCLIB_API_902) || defined (GHCLIB_API_900)
import GHC.Types.SrcLoc
import GHC.Driver.Session
import GHC.Parser.Lexer
# if !defined (GHCLIB_API_HEAD)
import GHC.Utils.Outputable
#endif
#  if !defined (GHCLIB_API_900)
import GHC.Driver.Ppr
#    if !defined (GHCLIB_API_HEAD) && !defined (GHCLIB_API_904)
import GHC.Parser.Errors.Ppr
#    endif
#  endif
import GHC.Utils.Error
import GHC.Types.Name.Reader
import GHC.Types.Name.Occurrence
#else
import SrcLoc
import DynFlags
import Lexer
import Outputable
import ErrUtils
import RdrName
import OccName
#endif
import GHC.LanguageExtensions.Type
#if defined (GHCLIB_API_808)
import Bag
#endif

basicDynFlags :: DynFlags
basicDynFlags =
  defaultDynFlags fakeSettings
#if !defined (GHCLIB_API_HEAD)
                                fakeLlvmConfig
#endif

main :: IO ()
main = do
  setEnv "TASTY_NUM_THREADS" "1"
  setUnsafeGlobalDynFlags basicDynFlags
  defaultMain tests

tests :: TestTree
tests = testGroup " All tests"
  [ parseTests
  , fixityTests
  , extendInstancesTests
  , expressionPredicateTests
  , typePredicateTests
  , patternPredicateTests
  , dynFlagsTests
  , nameTests
  ]

makeFile :: FilePath -> String -> IO FilePath
makeFile relPath contents = do
    Directory.createDirectoryIfMissing True $ FilePath.takeDirectory relPath
    writeFile relPath contents
    return relPath

#if defined(GHCLIB_API_HEAD) || defined (GHCLIB_API_904)
report :: DynFlags -> Bag (MsgEnvelope GhcMessage) -> String
report flags msgs = concat [ showSDoc flags msg | msg <- pprMsgEnvelopeBagWithLoc msgs ]
#elif defined (GHCLIB_API_902)
report :: DynFlags -> Bag (MsgEnvelope DecoratedSDoc) -> String
report flags msgs = concat [ showSDoc flags msg | msg <- pprMsgEnvelopeBagWithLoc msgs ]
#else
report :: DynFlags -> WarningMessages -> String
report flags msgs = concat [ showSDoc flags msg | msg <- pprErrMsgBagWithLoc msgs ]
#endif

chkParseResult :: DynFlags -> ParseResult a -> IO ()
chkParseResult flags = \case
    POk s _ -> do
#if defined (GHCLIB_API_HEAD) || defined (GHCLIB_API_904)
      let (wrns, errs) = getPsMessages s
#elif defined (GHCLIB_API_902)
      let (wrns, errs) = getMessages s
#else
      let (wrns, errs) = getMessages s flags
#endif
      when (not (null errs) || not (null wrns)) $
#if defined (GHCLIB_API_HEAD) || defined (GHCLIB_API_904)
        assertFailure (
          report flags (getMessages (GhcPsMessage <$> wrns)) ++
          report flags (getMessages (GhcPsMessage <$> errs))
        )
#elif defined (GHCLIB_API_902)
        assertFailure (report flags (fmap pprWarning wrns) ++ report flags (fmap pprError errs))
#else
        assertFailure (report flags wrns ++ report flags errs)
#endif
#if defined (GHCLIB_API_HEAD) || defined (GHCLIB_API_904)
    PFailed s -> assertFailure (report flags $ getMessages (GhcPsMessage <$> snd (getPsMessages s)))
#elif defined (GHCLIB_API_902)
    PFailed s -> assertFailure (report flags $ fmap pprError (snd (getMessages s)))
#elif defined (GHCLIB_API_900) || defined (GHCLIB_API_810)
    PFailed s -> assertFailure (report flags $ snd (getMessages s flags))
#else
    PFailed _ loc err -> assertFailure (report flags $ unitBag $ mkPlainErrMsg flags loc err)
#endif

parseTests :: TestTree
parseTests = testGroup "Parse tests"
  [
    testCase "Module" $
      chkParseResult flags (
        parseModule (unlines
          [ "module Foo (readMany) where"
          , "import Data.List"
          , "import Data.Maybe"
          , "readMany = unfoldr $ listToMaybe . concatMap reads . tails"
          ]) flags)
  , testCase "Module" $
      chkParseResult flags (
        parseModule (unlines
          [ "module Foo (readMany) where"
          , "import Data.List"
          , "import Data.Maybe"
          , "readMany = unfoldr $ listToMaybe . concatMap reads . tails"
          ]) flags)
  , testCase "Signature" $
      chkParseResult flags (
        parseSignature (unlines
          [ "signature Str where"
          , "data Str"
          , "empty :: Str"
          , "append :: Str -> Str -> Str"
          ]) flags)
  , testCase "Import" $
      chkParseResult flags (
        parseImport "import qualified \"foo-lib\" Foo as Bar hiding ((<.>))" flags)
  , testCase "Statement" $
      chkParseResult flags (
        parseStatement "Foo foo <- bar" flags)
  , testCase "Backpack" $
      chkParseResult flags (
        parseBackpack (unlines
          [ "unit main where"
          , "  module Main where"
          , "    main = putStrLn \"Hello world!\""
          ]) flags)
  , testCase "Expression" $
      chkParseResult flags (
        parseExpression "unfoldr $ listToMaybe . concatMap reads . tails" flags)
  , testCase "Declaration (1)" $
      chkParseResult flags (
        parseDeclaration "fact n = if n <= 1 then 1 else n * fact (n - 1)" flags)
  , testCase "Declaration (2)" $ -- Example from https://github.com/ndmitchell/hlint/issues/842.
      chkParseResult flags (
        parseDeclaration "infixr 4 <%@~" flags)
  , testCase "File" $ withTempDir $ \tmpDir -> do
      foo <- makeFile (tmpDir </> "Foo.hs") $ unlines
        ["{-# LANGUAGE ScopedTypeVariables #-}"
        , "module Foo (readMany) where"
        , "import Data.List"
        , "import Data.Maybe"
        , "readMany = unfoldr $ listToMaybe . concatMap reads . tails"
        ]
      s <- readFile' foo
      parsePragmasIntoDynFlags flags ([], []) foo s >>= \case
        Left msg -> assertFailure msg
        Right flags -> chkParseResult flags (parseFile foo flags s)
  ]
  where
    flags = basicDynFlags

#if defined (GHCLIB_API_904) || defined (GHCLIB_API_902) || defined (GHCLIB_API_900)
moduleTest :: String -> DynFlags -> (Located HsModule -> IO ()) -> IO ()
#else
moduleTest :: String -> DynFlags -> (Located (HsModule GhcPs) -> IO ()) -> IO ()
#endif
moduleTest s flags test =
      case parseModule s flags of
        POk _ e -> test e
        _ -> assertFailure "parse error"

exprTest :: String -> DynFlags -> (LHsExpr GhcPs -> IO ()) -> IO ()
exprTest s flags test =
      case parseExpression s flags of
        POk _ e -> test e
        _ -> assertFailure "parse error"

typeTest :: String -> DynFlags -> (LHsType GhcPs -> IO ()) -> IO ()
typeTest s flags test =
      case parseType s flags of
        POk _ e -> test e
        _ -> assertFailure "parse error"

patTest :: String -> DynFlags -> (LPat GhcPs -> IO ()) -> IO ()
patTest s flags test =
      case parsePattern s flags of
        POk _ e -> test e
        _ -> assertFailure "parse error"

fixityTests :: TestTree
fixityTests = testGroup "Fixity tests"
  [ testCase "Expression" $
      exprTest "1 + 2 * 3" flags
        (\e ->
            assertBool "parse tree not affected" $
#if defined (GHCLIB_API_HEAD) || defined (GHCLIB_API_904) || defined (GHCLIB_API_902)
              showSDocUnsafe (showAstData BlankSrcSpan BlankEpAnnotations e) /=
              showSDocUnsafe (showAstData BlankSrcSpan BlankEpAnnotations (applyFixities [] e))
#else
              showSDocUnsafe (showAstData BlankSrcSpan e) /=
              showSDocUnsafe (showAstData BlankSrcSpan (applyFixities [] e))
#endif
        )
  , testCase "Pattern" $
      case parseDeclaration "f (1 : 2 :[]) = 1" flags of
        POk _ d ->
          assertBool "parse tree not affected" $
#if defined (GHCLIB_API_HEAD) || defined (GHCLIB_API_904) || defined (GHCLIB_API_902)
          showSDocUnsafe (showAstData BlankSrcSpan BlankEpAnnotations d) /=
          showSDocUnsafe (showAstData BlankSrcSpan BlankEpAnnotations (applyFixities [] d))
#else
          showSDocUnsafe (showAstData BlankSrcSpan d) /=
          showSDocUnsafe (showAstData BlankSrcSpan (applyFixities [] d))
#endif
        PFailed{} -> assertFailure "parse error"
  , testCase "fixitiesFromModule" $
      case parseModule "infixl 4 <*!" flags of
        POk _ m ->
          assertBool "one fixity expected" $ not (null (fixitiesFromModule m))
        PFailed{} -> assertFailure "parse error"
  ]
  where
    flags = basicDynFlags

extendInstancesTests :: TestTree
extendInstancesTests = testGroup "Extend instances tests"
  [ testCase "Eq, Ord" $
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
  where
    flags = basicDynFlags

typePredicateTests :: TestTree
typePredicateTests = testGroup "Type predicate tests"
  [ testCase "isKindTyApp" $ test_with_exts [TypeApplications] "K @T" $ assert' . isKindTyApp
  , testCase "isKindTyApp" $ test_with_exts [TypeApplications] "K T" $ assert' . not . isKindTyApp
  ]
  where
    assert' = assertBool ""
    test_with_exts exts s = typeTest s (flags exts)
    flags = foldl' xopt_set basicDynFlags

expressionPredicateTests :: TestTree
expressionPredicateTests = testGroup "Expression predicate tests"
  [ testCase "isTag" $ test "foo" $ assert' . isTag "foo"
  , testCase "isTag" $ test "bar" $ assert' . not . isTag "foo"
  , testCase "isDol" $ test "f $ x" $ \case L _ (OpApp _ _ op _) -> assert' $ isDol op; _ -> assertFailure "unexpected"
  , testCase "isDot" $ test "f . g" $ \case L _ (OpApp _ _ op _) -> assert' $ isDot op; _ -> assertFailure "unexpected"
  , testCase "isReturn" $ test "return x" $ \case L _ (HsApp _ f _) -> assert' $ isReturn f; _ -> assertFailure "unexpected"
  , testCase "isReturn" $ test "pure x" $ \case L _ (HsApp _ f _) -> assert' $ isReturn f; _ -> assertFailure "unexpected"
#if defined (GHCLIB_API_HEAD) || defined (GHCLIB_API_904)
  , testCase "isSection" $ test "(1 +)" $ \case L _ (HsPar _ _ x _) -> assert' $ isSection x; _ -> assertFailure "unexpected"
#else
  , testCase "isSection" $ test "(1 +)" $ \case L _ (HsPar _ x) -> assert' $ isSection x; _ -> assertFailure "unexpected"
#endif
#if defined (GHCLIB_API_HEAD) || defined (GHCLIB_API_904)
  , testCase "isSection" $ test "(+ 1)" $ \case L _ (HsPar _ _ x _) -> assert' $ isSection x; _ -> assertFailure "unexpected"
#else
  , testCase "isSection" $ test "(+ 1)" $ \case L _ (HsPar _ x) -> assert' $ isSection x; _ -> assertFailure "unexpected"
#endif
  , testCase "isRecConstr" $ test "Foo {bar=1}" $ assert' . isRecConstr
  , testCase "isRecUpdate" $ test "foo {bar=1}" $ assert' . isRecUpdate
  , testCase "isVar" $ test "foo" $ assert' . isVar
  , testCase "isVar" $ test "3" $ assert' . not. isVar
  , testCase "isPar" $ test "(foo)" $ assert' . isPar
  , testCase "isPar" $ test "foo" $ assert' . not . isPar
  , testCase "isApp" $ test "f x" $ assert' . isApp
  , testCase "isApp" $ test "x" $ assert' . not . isApp
  , testCase "isOpApp" $ test "l `op` r" $ assert' . isOpApp
  , testCase "isOpApp" $ test "op l r" $ assert' . not . isOpApp
  , testCase "isAnyApp" $ test "l `op` r" $ assert' . isAnyApp
  , testCase "isAnyApp" $ test "f x" $ assert' . isAnyApp
  , testCase "isAnyApp" $ test "f x y" $ assert' . isAnyApp
  , testCase "isAnyApp" $ test "(f x y)" $ assert' . not . isAnyApp
  , testCase "isLexeme" $ test "foo" $ assert' . isLexeme
  , testCase "isLexeme" $ test "3" $ assert' . isLexeme
  , testCase "isLexeme" $ test "f x" $ assert' . not . isLexeme
  , testCase "isLambda" $ test "\\x -> 12" $ assert' . isLambda
  , testCase "isLambda" $ test "foo" $ assert' . not . isLambda
  , testCase "isDotApp" $ test "f . g" $ assert' . isDotApp
  , testCase "isDotApp" $ test "f $ g" $ assert' . not . isDotApp
  , testCase "isTypeApp" $ test "f @Int" $ assert' . isTypeApp
#if defined (GHCLIB_API_808) || defined (GHCLIB_API_810)
  , testCase "isTypeApp" $ test "f @ Int" $ assert' . isTypeApp
#else
  , testCase "isTypeApp" $ test "f @ Int" $ assert' . not . isTypeApp
#endif
  , testCase "isTypeApp" $ test "f" $ assert' . not . isTypeApp
  , testCase "isWHNF" $ test "[]" $ assert' . isWHNF
  , testCase "isWHNF" $ test "[1, 2]" $ assert' . isWHNF
  , testCase "isWHNF" $ test "'f'" $ assert' . isWHNF
  , testCase "isWHNF" $ test "foo" $ assert' . not . isWHNF
  , testCase "isWHNF" $ test "42" $ assert' . not . isWHNF
  , testCase "isWHNF" $ test "\\foo -> []" $ assert' . isWHNF
  , testCase "isWHNF" $ test "(\\foo -> [])" $ assert' . isWHNF
  , testCase "isWHNF" $ test "(\\foo -> []) x" $ assert' . not . isWHNF
  , testCase "isWHNF" $ test "(42, \"foo\")" $ assert' . isWHNF
  , testCase "isWHNF" $ test "(42, \"foo\") :: (Int, String)" $ assert' . isWHNF
  , testCase "isWHNF" $ test "(\\x -> x * x) 3 :: Int" $ assert' . not . isWHNF
  , testCase "isWHNF" $ test "Just foo" $ assert' . isWHNF
  , testCase "isWHNF" $ test "Left foo" $ assert' . isWHNF
  , testCase "isWHNF" $ test "Right foo" $ assert' . isWHNF
  , testCase "isWHNF" $ test "POk s" $ assert' . not . isWHNF
  , testCase "isLCase" $ test "\\case _ -> False" $ assert' . isLCase
  , testCase "isLCase" $ test "case x of _ -> False" $ assert' . not . isLCase
  , testCase "isSpliceDecl" $ test "$x" $ assert' . isSpliceDecl . unLoc
  , testCase "isSpliceDecl" $ test "f$x" $ assert' . not . isSpliceDecl . unLoc
  , testCase "isSpliceDecl" $ test "$(a + b)" $ assert' . isSpliceDecl . unLoc
  , testCase "isQuasiQuoteExpr" $ test "[expr|1 + 2|]" $ assert' . isQuasiQuoteExpr
  , testCase "isQuasiQuoteExpr" $ test "[expr(1 + 2)]" $ assert' . not . isQuasiQuoteExpr
  , testCase "isWholeFrac" $ test "3.2e1" $ assert' . isWholeFrac . unLoc
  , testCase "isWholeFrac" $ test "3.22e1" $ assert' . not . isWholeFrac . unLoc
  , testCase "isMDo" $ test_with_exts [ RecursiveDo ] "mdo { pure () }" $ assert' . any isMDo . universeBi
  , testCase "isListComp (1)" $ test "[ x + y | x <- xs, y <- ys ]" $ assert' . any isListComp . universeBi
  , testCase "isListComp (2)" $ test_with_exts [ MonadComprehensions ] "[ x + y | x <- xs, y <- ys ]" $ assert' . any isMonadComp . universeBi
  , testCase "isMonadComp (0)" $ test_with_exts [ MonadComprehensions ] "[ x + y | x <- Just 1, y <- Just 2 ]" $ assert' . not . any isListComp . universeBi
  , testCase "isMonadComp (1)" $ test_with_exts [ MonadComprehensions ] "[ x + y | x <- Just 1, y <- Just 2 ]" $ assert' . any isMonadComp . universeBi
  , testCase "isMonadComp (2)" $ test_with_exts [] "[ x + y | x <- Just 1, y <- Just 2 ]" $ assert' . not . any isMonadComp . universeBi
  , testCase "isMonadComp (3)" $ test_with_exts [] "[ x + y | x <- Just 1, y <- Just 2 ]" $ assert' . any isListComp . universeBi
  , testCase "strToVar" $ assert' . isVar . strToVar $ "foo"
  , testCase "varToStr" $ test "[]" $ assert' . (== "[]") . varToStr
  , testCase "varToStr" $ test "foo" $ assert' . (== "foo") . varToStr
  , testCase "varToStr" $ test "3" $ assert' . null . varToStr
  ]
  where
    assert' = assertBool ""
    test s = exprTest s (flags [])
    test_with_exts exts s = exprTest s (flags exts)
    flags exts = foldl' xopt_set basicDynFlags
              (exts ++
                 [ TemplateHaskell
                 , TemplateHaskellQuotes
                 , QuasiQuotes
                 , TypeApplications
                 , LambdaCase
                 ]
              )

patternPredicateTests :: TestTree
patternPredicateTests = testGroup "Pattern predicate tests"
  [ testCase "patToStr" $ test "True" $ assert' . (== "True") . patToStr
  , testCase "patToStr" $ test "False" $ assert' . (== "False") . patToStr
  , testCase "patToStr" $ test "[]" $ assert' . (== "[]") . patToStr
  , testCase "strToPat" $ assert' . (== "True") . patToStr . strToPat $ "True"
  , testCase "strToPat" $ assert' . (== "False") . patToStr . strToPat $ "False"
  , testCase "strToPat" $ assert' . (== "[]") . patToStr . strToPat $ "[]"
  , testCase "fromPChar" $ test "'a'" $ assert' . (== Just 'a') . fromPChar
  , testCase "fromPChar" $ test "\"a\"" $ assert' . isNothing . fromPChar
  , testCase "isSplicePat" $ test "$(varP pylonExPtrVarName)" $ assert' . isSplicePat
  ]
  where
    assert' = assertBool ""
    test = test_with_exts []
    test_with_exts exts s = patTest s (flags exts)
    flags exts = foldl' xopt_set basicDynFlags
              (exts ++
                 [ TemplateHaskell
                 , TemplateHaskellQuotes
                 , QuasiQuotes
                 , TypeApplications
                 , LambdaCase
                 ]
              )

dynFlagsTests :: TestTree
dynFlagsTests = testGroup "DynFlags tests"
  [ testCase "readExtension" $ assertBool "parse DeriveTraversable" (readExtension "DeriveTraversable" == Just DeriveTraversable)
  , testCase "readExtension" $ assertBool "parse DeriveTravresable" (isNothing $ readExtension "DeriveTravresable")
  , testCase "extensionImplications" $ do
      Just (_, (es, ds)) <- return $ find (\(e, _) -> e == DeriveTraversable) extensionImplications
      assertBool "no extensions disabled" (null ds)
      assertBool "two extensions enabled" $ DeriveFunctor `elem` es && DeriveFoldable `elem` es
  , testCase "check instance Bounded Language" $ assertBool "enumerate is null" (not (null (enumerate @Language)))
  , testCase "check instance Ord Extension" $ assertBool "minBound >= maxBound" (minBound @Extension < maxBound @Extension)
  , testCase "disable via pragma" $ withTempDir $ \tmpDir -> do
      foo <- makeFile (tmpDir </> "Foo.hs") $ unlines
        [ "{-# LANGUAGE NoStarIsType #-}"
        , "{-# LANGUAGE ExplicitNamespaces #-}"
        , "import GHC.TypeLits(KnownNat, type (+), type (*))"
        ]
      s <- readFile' foo
      -- If 'StarIsType' ends up enabled after
      -- 'parsePragmasIntoDynflags' has done its work, we'll get a
      -- parse error (see
      -- https://github.com/ndmitchell/hlint/issues/971).
      parsePragmasIntoDynFlags flags ([StarIsType], []) foo s >>= \case
        Left msg -> assertFailure msg
        Right flags -> chkParseResult flags (parseFile foo flags s)
#if defined (MIN_VERSION_ghc_lib_parser)
#  if !MIN_VERSION_ghc_lib_parser(1,  0,  0) || MIN_VERSION_ghc_lib_parser(8, 10, 0)
  , testCase "ImportQualifiedPost" $ do
      case parseImport "import Foo qualified" (flags `xopt_set` ImportQualifiedPost) of
        POk _ (L _ decl) -> assertBool "expected postpositive" (isImportQualifiedPost . ideclQualified $ decl)
        PFailed _ -> assertFailure "parse error"
#  endif
#elif __GLASGOW_HASKELL__ >= 810
  , testCase "ImportQualifiedPost" $ do
      case parseImport "import Foo qualified" (flags `xopt_set` ImportQualifiedPost) of
        POk _ (L _ decl) -> assertBool "expected postpositive" (isImportQualifiedPost . ideclQualified $ decl)
        PFailed _ -> assertFailure "parse error"
#endif
  ]
  where
    flags = basicDynFlags

nameTests :: TestTree
nameTests = testGroup "Name tests"
  [ testCase "modName (1)" $
      moduleTest "module Foo.Bar.Baz where" flags
        (\n -> assertBool "Unexpected name string" $ modName n == "Foo.Bar.Baz")
  , testCase "modName (2)" $
      moduleTest "f x = x * 2" flags
        (\n -> assertBool "Unexpected name string" $ modName n == "Main")
  , testCase "isSymbolRdrName (1)" $ assertBool "Expected 'True'" $ isSymbolRdrName (mkRdrUnqual (mkVarOcc "+"))
  , testCase "isSymbolRdrName (2)" $ assertBool "Expected 'False'" $ not (isSymbolRdrName (mkRdrUnqual (mkVarOcc "_foo")))
  , testCase "isSymbolRdrName (3)" $ assertBool "Expected 'False'" $ not (isSymbolRdrName (mkRdrUnqual (mkVarOcc "foo'")))
  , testCase "isSymbolRdrName (4)" $ assertBool "Expected 'True'" $ isSymbolRdrName (mkRdrUnqual (mkVarOcc ":+:"))
  ]
  where
    flags = basicDynFlags
