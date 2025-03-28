-- Copyright (c) 2020-2024, Shayne Fletcher. All rights reserved.
-- SPDX-License-Identifier: BSD-3-Clause.

{- ORMOLU_DISABLE -}
{-# OPTIONS_GHC -Wno-missing-fields #-}
{-# LANGUAGE LambdaCase #-}
#include "ghclib_api.h"
module Language.Haskell.GhclibParserEx.GHC.Hs.Expr(
  isTag, isDol, isDot, isReturn, isSection, isRecConstr, isRecUpdate,
  isVar, isPar, isApp, isOpApp, isAnyApp, isDo, isLexeme, isLambda, isQuasiQuote, isQuasiQuoteExpr, isQuasiQuoteSplice,  isOverLabel,
  isDotApp, isTypeApp, isWHNF, isLCase,
  isFieldPun, isFieldPunUpdate, isRecStmt, isLetStmt, isParComp, isMDo, isListComp, isMonadComp, isTupleSection, isString, isPrimLiteral,
  isSpliceDecl,
#if !( defined (GHC_9_4) || defined (GHC_9_2) || defined (GHC_9_0) || defined (GHC_8_10) || defined (GHC_8_8) )
  -- ghc api >= 9.6.1
  isTypedSplice, isUntypedSplice,
#endif
  isFieldWildcard, isUnboxed, isWholeFrac, isStrictMatch, isMultiIf, isProc, isTransStmt,
  hasFieldsDotDot,
  varToStr, strToVar,
  fromChar
  ) where

#if defined (GHC_8_8)
import HsSyn
import SrcLoc
import RdrName
import OccName
import Name
import BasicTypes
import TysWiredIn
#elif defined (GHC_8_10)
import GHC.Hs
import SrcLoc
import RdrName
import OccName
import Name
import BasicTypes
import TysWiredIn
#elif defined (GHC_9_0)
import GHC.Hs
import GHC.Types.SrcLoc
import GHC.Types.Name.Reader
import GHC.Types.Name
import GHC.Types.Basic
import GHC.Builtin.Types
#elif defined (GHC_9_2) || defined (GHC_9_4)
import GHC.Hs
import GHC.Types.SourceText
import GHC.Types.SrcLoc
import GHC.Types.Name.Reader
import GHC.Types.Name
import GHC.Types.Basic
import GHC.Builtin.Types
#else
import GHC.Hs
import GHC.Types.SourceText
import GHC.Types.SrcLoc
import GHC.Types.Name.Reader
import GHC.Types.Name
import GHC.Types.Basic
import GHC.Builtin.Types
import Language.Haskell.GhclibParserEx.GHC.Types.Name.Reader
#endif
import Data.Ratio

-- 'True' if the provided expression is a variable with name 'tag'.
isTag :: String -> LHsExpr GhcPs -> Bool
isTag tag = \case (L _ (HsVar _ (L _ s))) -> occNameString (rdrNameOcc s) == tag; _ -> False

isDot, isDol, isReturn, isSection, isRecConstr, isRecUpdate,
  isVar, isPar, isApp, isOpApp, isAnyApp, isDo, isLexeme, isQuasiQuote, isQuasiQuoteExpr,
  isLambda, isDotApp, isTypeApp, isWHNF, isLCase, isOverLabel :: LHsExpr GhcPs -> Bool
isDol = isTag "$"
isDot = isTag "."
isReturn x = isTag "return" x || isTag "pure" x -- Allow both 'pure' and 'return' as they have the same semantics.
isSection = \case (L _ SectionL{}) -> True ; (L _ SectionR{}) -> True; _ -> False
isRecConstr = \case (L _ RecordCon{}) -> True; _ -> False
isRecUpdate = \case (L _ RecordUpd{}) -> True; _ -> False
isVar = \case (L _ HsVar{}) -> True; _ -> False
isPar = \case (L _ HsPar{}) -> True; _ -> False
isApp = \case (L _ HsApp{}) -> True; _ -> False
isOpApp = \case (L _ OpApp{}) -> True; _ -> False
isAnyApp x = isApp x || isOpApp x
isDo = \case (L _ HsDo{}) -> True; _ -> False
isLexeme = \case (L _ HsVar{}) -> True; (L _ HsOverLit{}) -> True; (L _ HsLit{}) -> True; _ -> False
-- 'isLambda' semantics are match form `\p -> e` exclusively
#if ! ( defined (GHC_9_8) || defined (GHC_9_6) || defined (GHC_9_4) || defined (GHC_9_2) || defined (GHC_9_0) || defined (GHC_8_10) || defined (GHC_8_8) )
--ghc api >= 9.8.1
isLambda = \case (L _ (HsLam _ LamSingle _)) -> True; _ -> False
#else
isLambda = \case (L _ HsLam{}) -> True; _ -> False
#endif
#if ! ( defined (GHC_9_4) || defined (GHC_9_2) || defined (GHC_9_0) || defined (GHC_8_10) || defined (GHC_8_8) )
-- ghc api >= 9.6.1
isQuasiQuoteExpr = \case (L _ (HsUntypedSplice _ HsQuasiQuote{})) -> True; _ -> False
#else
-- ghc api < 9.6.1
isQuasiQuoteExpr = \case (L _ (HsSpliceE _ HsQuasiQuote{})) -> True; _ -> False
#endif
isQuasiQuote = isQuasiQuoteExpr -- Backwards compat.
isDotApp = \case (L _ (OpApp _ _ op _)) -> isDot op; _ -> False
isTypeApp = \case (L _ HsAppType{}) -> True; _ -> False
isWHNF = \case
  (L _ (HsVar _ (L _ x))) -> isRdrDataCon x
#if ! ( defined(GHC_9_12) || defined (GHC_9_10) || defined (GHC_9_8) || defined(GHC_9_6) || defined (GHC_9_4) || defined (GHC_9_2) || defined (GHC_9_0) || defined (GHC_8_10) || defined (GHC_8_8) )
-- ghc api > 9.12.1
  (L _ (HsLit _ x)) -> case x of HsString{} -> False; _ -> True
#else
  (L _ (HsLit _ x)) -> case x of HsString{} -> False; HsInt{} -> False; HsRat{} -> False; _ -> True
#endif
  (L _ HsLam{}) -> True
  (L _ ExplicitTuple{}) -> True
  (L _ ExplicitList{}) -> True

#if ! ( defined (GHC_9_8) || defined (GHC_9_6) || defined (GHC_9_4) || defined (GHC_9_2) || defined (GHC_9_0) || defined (GHC_8_10) || defined (GHC_8_8) )
-- ghc api >= 9.8
  (L _ (HsPar _  x )) -> isWHNF x
#elif ! ( defined (GHC_9_2) || defined (GHC_9_0) || defined (GHC_8_10) || defined (GHC_8_8) )
-- ghc api >= 9.4
  (L _ (HsPar _ _ x _)) -> isWHNF x
#else
  (L _ (HsPar _ x)) -> isWHNF x
#endif
  (L _ (ExprWithTySig _ x _)) -> isWHNF x
  -- Other (unknown) constructors may have bang patterns in them, so
  -- approximate.
  (L _ (HsApp _ (L _ (HsVar _ (L _ x))) _))
    | occNameString (rdrNameOcc x) `elem` ["Just", "Left", "Right"] -> True
  _ -> False
#if ! ( defined (GHC_9_8) || defined (GHC_9_6) || defined (GHC_9_4) || defined (GHC_9_2) || defined (GHC_9_0) || defined (GHC_8_10) || defined (GHC_8_8) )
isLCase = \case (L _ (HsLam _ LamCase _)) -> True; _ -> False
#else
isLCase = \case (L _ HsLamCase{}) -> True; _ -> False
#endif
isOverLabel = \case (L _ HsOverLabel{}) -> True; _ -> False

#if ! ( defined (GHC_9_4) || defined (GHC_9_2) || defined (GHC_9_0) || defined (GHC_8_10) || defined (GHC_8_8) )
-- ghc api >= 9.6.1
isQuasiQuoteSplice :: HsUntypedSplice GhcPs -> Bool
#else
isQuasiQuoteSplice :: HsSplice GhcPs -> Bool
#endif
isQuasiQuoteSplice = \case HsQuasiQuote{} -> True; _ -> False

#if ( defined (GHC_8_10) || defined (GHC_8_8) )
isStrictMatch :: HsMatchContext RdrName -> Bool
#elif ( defined (GHC_9_8) || defined (GHC_9_6) || defined (GHC_9_4) || defined (GHC_9_2) || defined (GHC_9_0) )
isStrictMatch :: HsMatchContext GhcPs -> Bool
#else
-- ghc > 9.8.1
isStrictMatch :: HsMatchContext (LocatedN RdrName) -> Bool
#endif
isStrictMatch = \case FunRhs{mc_strictness=SrcStrict} -> True; _ -> False

-- Field is punned e.g. '{foo}'.
#if ! ( defined (GHC_9_2) || defined (GHC_9_0) || defined (GHC_8_10) || defined (GHC_8_8) )
-- ghc api >= 9.4.1
isFieldPun :: LHsFieldBind GhcPs (LFieldOcc GhcPs) (LHsExpr GhcPs) -> Bool
isFieldPun = \case (L _ HsFieldBind {hfbPun=True}) -> True; _ -> False
#else
isFieldPun :: LHsRecField GhcPs (LHsExpr GhcPs) -> Bool
isFieldPun = \case (L _ HsRecField {hsRecPun=True}) -> True; _ -> False
#endif
-- Field puns in updates have a different type to field puns in
-- constructions.
#if ! ( defined (GHC_9_10) || defined (GHC_9_8) || defined(GHC_9_6) || defined (GHC_9_4) || defined (GHC_9_2) || defined (GHC_9_0) || defined (GHC_8_10) || defined (GHC_8_8) )
-- ghc api > 9.10.1
isFieldPunUpdate :: HsFieldBind (LFieldOcc GhcPs) (LHsExpr GhcPs) -> Bool
isFieldPunUpdate = \case HsFieldBind {hfbPun=True} -> True; _ -> False
#elif ! ( defined (GHC_9_2) || defined (GHC_9_0) || defined (GHC_8_10) || defined (GHC_8_8) )
-- ghc api >= 9.4.1 && <= 9.12.1
isFieldPunUpdate :: HsFieldBind (LAmbiguousFieldOcc GhcPs) (LHsExpr GhcPs) -> Bool
isFieldPunUpdate = \case HsFieldBind {hfbPun=True} -> True; _ -> False
#else
isFieldPunUpdate :: HsRecField' (AmbiguousFieldOcc GhcPs) (LHsExpr GhcPs) -> Bool
isFieldPunUpdate = \case HsRecField {hsRecPun=True} -> True; _ -> False
#endif

-- Contains a '..' as in 'Foo{..}'
hasFieldsDotDot :: HsRecFields GhcPs (LHsExpr GhcPs) -> Bool
hasFieldsDotDot = \case HsRecFields {rec_dotdot=Just _} -> True; _ -> False

isRecStmt :: StmtLR GhcPs GhcPs (LHsExpr GhcPs) -> Bool
isRecStmt = \case RecStmt{} -> True; _ -> False

isLetStmt :: StmtLR GhcPs GhcPs (LHsExpr GhcPs) -> Bool
isLetStmt = \case LetStmt{} -> True; _ -> False

isParComp :: StmtLR GhcPs GhcPs (LHsExpr GhcPs) -> Bool
isParComp = \case ParStmt{} -> True; _ -> False

-- TODO: Seems `HsStmtContext (HsDoRn p)` on master right now.
#if ! ( defined (GHC_9_2) || defined (GHC_9_0) || defined (GHC_8_10) || defined (GHC_8_8) )
-- ghc api >= 9.4.1
isMDo :: HsDoFlavour -> Bool
isMDo = \case MDoExpr _ -> True; _ -> False
isMonadComp :: HsDoFlavour -> Bool
isMonadComp = \case MonadComp -> True; _ -> False
isListComp :: HsDoFlavour -> Bool
isListComp = \case ListComp -> True; _ -> False
#elif defined (GHC_9_2) || defined (GHC_9_0)
isMDo :: HsStmtContext GhcRn -> Bool
isMDo = \case MDoExpr _ -> True; _ -> False
isMonadComp :: HsStmtContext GhcRn -> Bool
isMonadComp = \case MonadComp -> True; _ -> False
isListComp :: HsStmtContext GhcRn -> Bool
isListComp = \case ListComp -> True; _ -> False
#else
isMDo :: HsStmtContext Name -> Bool
isMDo = \case MDoExpr -> True; _ -> False
isMonadComp :: HsStmtContext Name -> Bool
isMonadComp = \case MonadComp -> True; _ -> False
isListComp :: HsStmtContext Name -> Bool
isListComp = \case ListComp -> True; _ -> False
#endif

isTupleSection :: HsTupArg GhcPs -> Bool
isTupleSection = \case Missing{} -> True; _ -> False

isString :: HsLit GhcPs -> Bool
isString = \case HsString{} -> True; _ -> False

isPrimLiteral :: HsLit GhcPs -> Bool
isPrimLiteral = \case
  HsCharPrim{} -> True
  HsStringPrim{} -> True
  HsIntPrim{} -> True
  HsWordPrim{} -> True
  HsInt64Prim{} -> True
  HsWord64Prim{} -> True
  HsFloatPrim{} -> True
  HsDoublePrim{} -> True
  _ -> False

-- Since ghc-9.6.1, `HsTypedSplice` and `HsUntypedSplice` have been
-- top-level constuctors of `Language.Haskell.Syntax.Expr.HsExpr p`
#if ! ( defined (GHC_9_4) || defined (GHC_9_2) || defined (GHC_9_0) || defined (GHC_8_10) || defined (GHC_8_8) )
-- ghc api >= ghc-9.6.1
isTypedSplice, isUntypedSplice :: HsExpr GhcPs -> Bool
isTypedSplice = \case HsTypedSplice{} -> True; _ -> False
isUntypedSplice = \case HsUntypedSplice{} -> True; _ -> False
#endif

isSpliceDecl :: HsExpr GhcPs -> Bool
#if ! ( defined (GHC_9_4) || defined (GHC_9_2) || defined (GHC_9_0) || defined (GHC_8_10) || defined (GHC_8_8) )
-- ghc api >= 9.6.1
isSpliceDecl = \case
  HsTypedSplice{} -> True
  HsUntypedSplice{} -> True
  _ -> False
#else
isSpliceDecl = \case HsSpliceE{} -> True; _ -> False
#endif

isMultiIf :: HsExpr GhcPs -> Bool
isMultiIf = \case HsMultiIf{} -> True; _ -> False

isProc :: HsExpr GhcPs -> Bool
isProc = \case HsProc{} -> True; _ -> False

isTransStmt :: StmtLR GhcPs GhcPs (LHsExpr GhcPs) -> Bool
isTransStmt = \case TransStmt{} -> True; _ -> False

-- Field has a '_' as in '{foo=_} or is punned e.g. '{foo}'.
#if ! ( defined (GHC_9_2) || defined (GHC_9_0) || defined (GHC_8_10) || defined (GHC_8_8) )
-- ghc api >= 9.4.1
isFieldWildcard :: LHsFieldBind GhcPs (LFieldOcc GhcPs) (LHsExpr GhcPs) -> Bool
#else
isFieldWildcard :: LHsRecField GhcPs (LHsExpr GhcPs) -> Bool
#endif
isFieldWildcard = \case
#if !(defined (GHC_9_12) || defined (GHC_9_10) || defined (GHC_9_8) || defined (GHC_9_6) || defined (GHC_9_4) || defined (GHC_9_2) || defined(GHC_9_0) || defined (GHC_8_10) || defined (GHC_8_8))
-- ghc api > ghc-9.12
-- Use `Language.Haskell.GhcLibParserEx.GHC.Types.Name.Reader`s `occNameStr`. `HsHoleVar` has a `RdrName` not an `OccName`.
  (L _ HsFieldBind {hfbRHS=(L _ (HsHole(HoleVar(L _ s))))}) -> occNameStr s == "_"
#elif defined(GHC_9_12) || defined(GHC_9_10) || defined (GHC_9_8) || defined(GHC_9_6)
-- ghc api >= ghc-9.6.1
-- Use `Language.Haskell.GhcLibParserEx.GHC.Types.Name.Reader`s `occNameStr` since `HsUnboundVar` now contains a `RdrName` not an `OccName`.
  (L _ HsFieldBind {hfbRHS=(L _ (HsUnboundVar _ s))}) -> occNameStr s == "_"
#elif defined (GHC_9_4)
  (L _ HsFieldBind {hfbRHS=(L _ (HsUnboundVar _ s))}) -> occNameString s == "_"
#elif defined (GHC_9_2) || defined (GHC_9_0)
  (L _ HsRecField {hsRecFieldArg=(L _ (HsUnboundVar _ s))}) -> occNameString s == "_"
#elif defined (GHC_8_10)
  (L _ HsRecField {hsRecFieldArg=(L _ (HsUnboundVar _ _))}) -> True
#else
  (L _ HsRecField {hsRecFieldArg=(L _ (EWildPat _))}) -> True
#endif
#if ! (defined (GHC_9_2) || defined (GHC_9_0) || defined (GHC_8_10) || defined (GHC_8_8) )
-- ghc api >= 9.4.1
  (L _ HsFieldBind {hfbPun=True}) -> True
  (L _ HsFieldBind {}) -> False
#else
  (L _ HsRecField {hsRecPun=True}) -> True
  (L _ HsRecField {}) -> False
#endif

isUnboxed :: Boxity -> Bool
isUnboxed = \case Unboxed -> True; _ -> False

isWholeFrac :: HsExpr GhcPs -> Bool

#if ! ( defined(GHC_9_12) || defined (GHC_9_10) || defined (GHC_9_8) || defined(GHC_9_6) || defined (GHC_9_4) || defined (GHC_9_2) || defined (GHC_9_0) || defined (GHC_8_10) || defined (GHC_8_8) )
-- ghc api > 9.12.1
isWholeFrac (HsOverLit _ (OverLit _ (HsFractional fl@FL {}) )) = denominator (rationalFromFractionalLit fl) == 1
#elif ! (defined (GHC_9_2) || defined (GHC_9_0) || defined (GHC_8_10) || defined (GHC_8_8) )
-- ghc api >= 9.4.1
isWholeFrac (HsLit _ (HsRat _ fl@FL{} _)) = denominator (rationalFromFractionalLit fl) == 1
isWholeFrac (HsOverLit _ (OverLit _ (HsFractional fl@FL {}) )) = denominator (rationalFromFractionalLit fl) == 1
#elif defined (GHC_9_2)
isWholeFrac (HsLit _ (HsRat _ fl@FL{} _)) = denominator (rationalFromFractionalLit fl) == 1
isWholeFrac (HsOverLit _ (OverLit _ (HsFractional fl@FL {}) _)) = denominator (rationalFromFractionalLit fl) == 1
#else
isWholeFrac (HsLit _ (HsRat _ (FL _ _ v) _)) = denominator v == 1
isWholeFrac (HsOverLit _ (OverLit _ (HsFractional (FL _ _ v)) _)) = denominator v == 1
#endif
isWholeFrac _ = False

varToStr :: LHsExpr GhcPs -> String
varToStr (L _ (HsVar _ (L _ n)))
  | n == consDataCon_RDR = ":"
  | n == nameRdrName nilDataConName = "[]"
  | n == nameRdrName (getName (tupleDataCon Boxed 0)) = "()"
  | otherwise = occNameString (rdrNameOcc n)
varToStr _ = ""

strToVar :: String -> LHsExpr GhcPs
#if ! ( defined (GHC_9_0) || defined (GHC_8_10) || defined (GHC_8_8) )
-- ghc api >= 9.2.1
strToVar x = noLocA $ HsVar noExtField (noLocA $ mkRdrUnqual (mkVarOcc x))
#elif defined (GHC_9_0) || defined (GHC_8_10)
strToVar x = noLoc $ HsVar noExtField (noLoc $ mkRdrUnqual (mkVarOcc x))
#else
strToVar x = noLoc $ HsVar noExt (noLoc $ mkRdrUnqual (mkVarOcc x))
#endif

fromChar :: LHsExpr GhcPs -> Maybe Char
fromChar = \case (L _ (HsLit _ (HsChar _ x))) -> Just x; _ -> Nothing
