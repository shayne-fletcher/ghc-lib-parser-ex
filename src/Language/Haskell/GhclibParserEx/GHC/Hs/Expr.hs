-- Copyright (c) 2020, Shayne Fletcher. All rights reserved.
-- SPDX-License-Identifier: BSD-3-Clause.

{-# OPTIONS_GHC -Wno-missing-fields #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE LambdaCase #-}
#include "ghclib_api.h"
module Language.Haskell.GhclibParserEx.GHC.Hs.Expr(
  isTag, isDol, isDot, isReturn, isSection, isRecConstr, isRecUpdate,
  isVar, isPar, isApp, isOpApp, isAnyApp, isLexeme, isLambda, isQuasiQuote,
  isDotApp, isTypeApp, isWHNF, isLCase,
  isFieldPun, isFieldPunUpdate, isRecStmt, isParComp, isMDo, isTupleSection, isString, isPrimLiteral,
  isSpliceDecl, isFieldWildcard, isUnboxed, isWholeFrac, isStrictMatch, isMultiIf, isProc, isTransStmt,
  hasFieldsDotDot,
  varToStr, strToVar,
  fromChar
  ) where

#if defined (GHCLIB_API_HEAD) || defined(GHCLIB_API_920) || defined (GHCLIB_API_900) || defined (GHCLIB_API_810)
import GHC.Hs
#else
import HsSyn
#endif
#if defined (GHCLIB_API_HEAD) || defined(GHCLIB_API_920) || defined (GHCLIB_API_900)
#if defined (GHCLIB_API_HEAD) || defined(GHCLIB_API_920)
import GHC.Types.SourceText
#endif
import GHC.Types.SrcLoc
import GHC.Types.Name.Reader
import GHC.Types.Name
import GHC.Types.Basic
import GHC.Builtin.Types
#else
import SrcLoc
import RdrName
import OccName
import Name
import BasicTypes
import TysWiredIn
#endif
import Data.Ratio

-- 'True' if the provided expression is a variable with name 'tag'.
isTag :: String -> LHsExpr GhcPs -> Bool
isTag tag = \case (L _ (HsVar _ (L _ s))) -> occNameString (rdrNameOcc s) == tag; _ -> False

isDot, isDol, isReturn, isSection, isRecConstr, isRecUpdate,
  isVar, isPar, isApp, isOpApp, isAnyApp, isLexeme, isQuasiQuote,
  isLambda, isDotApp, isTypeApp, isWHNF, isLCase :: LHsExpr GhcPs -> Bool
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
isLexeme = \case (L _ HsVar{}) -> True; (L _ HsOverLit{}) -> True; (L _ HsLit{}) -> True; _ -> False
isLambda = \case (L _ HsLam{}) -> True; _ -> False
isQuasiQuote = \case (L _ (HsSpliceE _ HsQuasiQuote{})) -> True; _ -> False
isDotApp = \case (L _ (OpApp _ _ op _)) -> isDot op; _ -> False
isTypeApp = \case (L _ HsAppType{}) -> True; _ -> False
isWHNF = \case
  (L _ (HsVar _ (L _ x))) -> isRdrDataCon x
  (L _ (HsLit _ x)) -> case x of HsString{} -> False; HsInt{} -> False; HsRat{} -> False; _ -> True
  (L _ HsLam{}) -> True
  (L _ ExplicitTuple{}) -> True
  (L _ ExplicitList{}) -> True
  (L _ (HsPar _ x)) -> isWHNF x
  (L _ (ExprWithTySig _ x _)) -> isWHNF x
  -- Other (unknown) constructors may have bang patterns in them, so
  -- approximate.
  (L _ (HsApp _ (L _ (HsVar _ (L _ x))) _))
    | occNameString (rdrNameOcc x) `elem` ["Just", "Left", "Right"] -> True
  _ -> False
isLCase = \case (L _ HsLamCase{}) -> True; _ -> False

#if defined (GHCLIB_API_HEAD) || defined(GHCLIB_API_920) || defined (GHCLIB_API_901)
isStrictMatch :: HsMatchContext GhcPs -> Bool
#else
isStrictMatch :: HsMatchContext RdrName -> Bool
#endif
isStrictMatch = \case FunRhs{mc_strictness=SrcStrict} -> True; _ -> False

-- Field is punned e.g. '{foo}'.
isFieldPun :: LHsRecField GhcPs (LHsExpr GhcPs) -> Bool
isFieldPun = \case (L _ HsRecField {hsRecPun=True}) -> True; _ -> False

-- Field puns in updates have a different type to field puns in
-- constructions.
isFieldPunUpdate :: HsRecField' (AmbiguousFieldOcc GhcPs) (LHsExpr GhcPs) -> Bool
isFieldPunUpdate = \case HsRecField {hsRecPun=True} -> True; _ -> False

-- Contains a '..' as in 'Foo{..}'
hasFieldsDotDot :: HsRecFields GhcPs (LHsExpr GhcPs) -> Bool
hasFieldsDotDot = \case HsRecFields {rec_dotdot=Just _} -> True; _ -> False

isRecStmt :: StmtLR GhcPs GhcPs (LHsExpr GhcPs) -> Bool
isRecStmt = \case RecStmt{} -> True; _ -> False

isParComp :: StmtLR GhcPs GhcPs (LHsExpr GhcPs) -> Bool
isParComp = \case ParStmt{} -> True; _ -> False

#if defined (GHCLIB_API_HEAD) || defined(GHCLIB_API_920) || defined (GHCLIB_API_900)
isMDo :: HsStmtContext GhcRn -> Bool
isMDo = \case MDoExpr _ -> True; _ -> False
#else
isMDo :: HsStmtContext Name -> Bool
isMDo = \case MDoExpr -> True; _ -> False
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

isSpliceDecl :: HsExpr GhcPs -> Bool
isSpliceDecl = \case HsSpliceE{} -> True; _ -> False

isMultiIf :: HsExpr GhcPs -> Bool
isMultiIf = \case HsMultiIf{} -> True; _ -> False

isProc :: HsExpr GhcPs -> Bool
isProc = \case HsProc{} -> True; _ -> False

isTransStmt :: StmtLR GhcPs GhcPs (LHsExpr GhcPs) -> Bool
isTransStmt = \case TransStmt{} -> True; _ -> False

-- Field has a '_' as in '{foo=_} or is punned e.g. '{foo}'.
isFieldWildcard :: LHsRecField GhcPs (LHsExpr GhcPs) -> Bool
isFieldWildcard = \case
#if defined (GHCLIB_API_HEAD) || defined(GHCLIB_API_920) || defined (GHCLIB_API_900)
  (L _ HsRecField {hsRecFieldArg=(L _ (HsUnboundVar _ s))}) -> occNameString s == "_"
#elif defined (GHCLIB_API_810)
  (L _ HsRecField {hsRecFieldArg=(L _ (HsUnboundVar _ _))}) -> True
#else
  (L _ HsRecField {hsRecFieldArg=(L _ (EWildPat _))}) -> True
#endif
  (L _ HsRecField {hsRecPun=True}) -> True
  (L _ HsRecField {}) -> False

isUnboxed :: Boxity -> Bool
isUnboxed = \case Unboxed -> True; _ -> False

isWholeFrac :: HsExpr GhcPs -> Bool
#if defined (GHCLIB_API_HEAD) || defined(GHCLIB_API_920)
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
#if defined (GHCLIB_API_HEAD) || defined(GHCLIB_API_920)
strToVar x = noLocA $ HsVar noExtField (noLocA $ mkRdrUnqual (mkVarOcc x))
#elif defined (GHCLIB_API_900) || defined (GHCLIB_API_810)
strToVar x = noLoc $ HsVar noExtField (noLoc $ mkRdrUnqual (mkVarOcc x))
#else
strToVar x = noLoc $ HsVar noExt (noLoc $ mkRdrUnqual (mkVarOcc x))
#endif

fromChar :: LHsExpr GhcPs -> Maybe Char
fromChar = \case (L _ (HsLit _ (HsChar _ x))) -> Just x; _ -> Nothing
