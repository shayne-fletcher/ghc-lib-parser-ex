-- Copyright (c) 2020-203, Shayne Fletcher. All rights reserved.
-- SPDX-License-Identifier: BSD-3-Clause.

{-# LANGUAGE CPP #-}
{-# LANGUAGE ViewPatterns #-}
#include "ghclib_api.h"
module Language.Haskell.GhclibParserEx.GHC.Hs.Pat(
    patToStr, strToPat
  , fromPChar
  , hasPFieldsDotDot
  , isPFieldWildcard, isPWildcard, isPFieldPun, isPatTypeSig, isPBangPat, isPViewPat
  , isSplicePat
 ) where

#if defined (GHC_9_10) || defined (GHC_9_8) || defined (GHC_9_6) || defined (GHC_9_4) || defined (GHCLIB_API_902) || defined (GHC_9_0) || defined (GHC_8_10)
import GHC.Hs
#else
import HsSyn
#endif
#if defined (GHC_9_10) || defined (GHC_9_8) || defined (GHC_9_6) || defined (GHC_9_4) || defined (GHCLIB_API_902) || defined (GHC_9_0)
import GHC.Types.SrcLoc
import GHC.Builtin.Types
import GHC.Types.Name.Reader
import GHC.Types.Name
import GHC.Data.FastString
#else
import SrcLoc
import TysWiredIn
import RdrName
import OccName
import FastString
#endif

patToStr :: LPat GhcPs -> String
#if defined (GHC_9_10) || defined (GHC_9_8) || defined (GHC_9_6) || defined (GHC_9_4) || defined (GHCLIB_API_902)
patToStr (L _ (ConPat _ (L _ x) (PrefixCon [] []))) | occNameString (rdrNameOcc x) == "True" = "True"
patToStr (L _ (ConPat _ (L _ x) (PrefixCon [] []))) | occNameString (rdrNameOcc x) == "False" = "False"
patToStr (L _ (ConPat _ (L _ x) (PrefixCon [] []))) | occNameString (rdrNameOcc x) == "[]" = "[]"
patToStr _ = ""
#elif defined (GHC_9_0)
patToStr (L _ (ConPat _ (L _ x) (PrefixCon []))) | occNameString (rdrNameOcc x) == "True" = "True"
patToStr (L _ (ConPat _ (L _ x) (PrefixCon []))) | occNameString (rdrNameOcc x) == "False" = "False"
patToStr (L _ (ConPat _ (L _ x) (PrefixCon []))) | occNameString (rdrNameOcc x) == "[]" = "[]"
patToStr _ = ""
#elif defined (GHC_8_10)
patToStr (L _ (ConPatIn (L _ x) (PrefixCon []))) | occNameString (rdrNameOcc x) == "True" = "True"
patToStr (L _ (ConPatIn (L _ x) (PrefixCon []))) | occNameString (rdrNameOcc x) == "False" = "False"
patToStr (L _ (ConPatIn (L _ x) (PrefixCon []))) | occNameString (rdrNameOcc x) == "[]" = "[]"
patToStr _ = ""
#else
patToStr (dL -> L _ (ConPatIn (L _ x) (PrefixCon []))) | occNameString (rdrNameOcc x) == "True" = "True"
patToStr (dL -> L _ (ConPatIn (L _ x) (PrefixCon []))) | occNameString (rdrNameOcc x) == "False" = "False"
patToStr (dL -> L _ (ConPatIn (L _ x) (PrefixCon []))) | occNameString (rdrNameOcc x) == "[]" = "[]"
patToStr _ = ""
#endif

strToPat :: String -> LPat GhcPs
strToPat z
  | z == "True"  =
#if defined (GHC_9_10) || defined (GHC_9_8) || defined (GHC_9_6) || defined (GHC_9_4) || defined (GHCLIB_API_902)
  noLocA $
#elif defined (GHC_9_0) || defined (GHC_8_10)
  noLoc $
#endif
#if defined (GHC_9_10) || defined (GHC_9_8) || defined (GHC_9_6) || defined (GHC_9_4) || defined (GHCLIB_API_902)
    ConPat noAnn (noLocA true_RDR) (PrefixCon [] [])
#elif defined (GHC_9_0)
    ConPat noExtField (noLoc true_RDR) (PrefixCon [])
#else
    ConPatIn (noLoc true_RDR) (PrefixCon [])
#endif
  | z == "False" =
#if defined (GHC_9_10) || defined (GHC_9_8) || defined (GHC_9_6) || defined (GHC_9_4) || defined (GHCLIB_API_902)
  noLocA $
#elif defined (GHC_9_0) || defined (GHC_8_10)
  noLoc $
#endif
#if defined (GHC_9_10) || defined (GHC_9_8) || defined (GHC_9_6) || defined (GHC_9_4) || defined (GHCLIB_API_902)
    ConPat noAnn (noLocA false_RDR) (PrefixCon [] [])
#elif defined (GHC_9_0)
    ConPat noExtField (noLoc false_RDR) (PrefixCon [])
#else
    ConPatIn (noLoc false_RDR) (PrefixCon [])
#endif
  | z == "[]"    =
#if defined (GHC_9_10) || defined (GHC_9_8) || defined (GHC_9_6) || defined (GHC_9_4) || defined (GHCLIB_API_902)
  noLocA $
#elif defined (GHC_9_0) || defined (GHC_8_10)
  noLoc $
#endif
#if defined (GHC_9_10) || defined (GHC_9_8) || defined (GHC_9_6) || defined (GHC_9_4) || defined (GHCLIB_API_902)
    ConPat noAnn (noLocA $ nameRdrName nilDataConName) (PrefixCon [] [])
#elif defined (GHC_9_0)
    ConPat noExtField (noLoc $ nameRdrName nilDataConName) (PrefixCon [])
#else
    ConPatIn (noLoc $ nameRdrName nilDataConName) (PrefixCon [])
#endif
  | otherwise =
#if defined (GHC_9_10) || defined (GHC_9_8) || defined (GHC_9_6) || defined (GHC_9_4) || defined (GHCLIB_API_902)
      noLocA $ VarPat noExtField (noLocA $ mkVarUnqual (fsLit z))
#elif defined (GHC_9_0) || defined (GHC_8_10)
      noLoc $ VarPat noExtField (noLoc $ mkVarUnqual (fsLit z))
#else
      VarPat noExt (noLoc $ mkVarUnqual (fsLit z))
#endif

fromPChar :: LPat GhcPs -> Maybe Char
#if defined (GHC_9_10) || defined (GHC_9_8) || defined (GHC_9_6) || defined (GHC_9_4) || defined (GHCLIB_API_902) || defined (GHC_9_0) || defined (GHC_8_10)
fromPChar (L _ (LitPat _ (HsChar _ x))) = Just x
#else
fromPChar (dL -> L _ (LitPat _ (HsChar _ x))) = Just x
#endif
fromPChar _ = Nothing

-- Contains a '..' as in 'Foo{..}'
hasPFieldsDotDot :: HsRecFields GhcPs (LPat GhcPs) -> Bool
hasPFieldsDotDot HsRecFields {rec_dotdot=Just _} = True
hasPFieldsDotDot _ = False

-- Field has a '_' as in '{foo=_} or is punned e.g. '{foo}'.
#if defined (GHC_9_10) || defined (GHC_9_8) || defined (GHC_9_6) || defined (GHC_9_4)
isPFieldWildcard :: LHsFieldBind GhcPs (LFieldOcc GhcPs) (LPat GhcPs) -> Bool
#else
isPFieldWildcard :: LHsRecField GhcPs (LPat GhcPs) -> Bool
#endif
#if defined (GHC_9_10) || defined (GHC_9_8) || defined (GHC_9_6) || defined (GHC_9_4)
isPFieldWildcard (L _ HsFieldBind {hfbRHS=L _ WildPat {}}) = True
isPFieldWildcard (L _ HsFieldBind {hfbPun=True}) = True
isPFieldWildcard (L _ HsFieldBind {}) = False
#elif defined (GHCLIB_API_902) || defined (GHC_9_0) || defined (GHC_8_10)
isPFieldWildcard (L _ HsRecField {hsRecFieldArg=L _ WildPat {}}) = True
isPFieldWildcard (L _ HsRecField {hsRecPun=True}) = True
isPFieldWildcard (L _ HsRecField {}) = False
#else
isPFieldWildcard (dL -> L _ HsRecField {hsRecFieldArg=LL _ WildPat {}}) = True
isPFieldWildcard (dL -> L _ HsRecField {hsRecPun=True}) = True
isPFieldWildcard (dL -> L _ HsRecField {}) = False
#endif

isPWildcard :: LPat GhcPs -> Bool
#if defined (GHC_9_10) || defined (GHC_9_8) || defined (GHC_9_6) || defined (GHC_9_4) || defined (GHCLIB_API_902) || defined (GHC_9_0) || defined (GHC_8_10)
isPWildcard (L _ (WildPat _)) = True
#else
isPWildcard (dL -> L _ (WildPat _)) = True
#endif
isPWildcard _ = False

#if defined (GHC_9_10) || defined (GHC_9_8) || defined (GHC_9_6) || defined (GHC_9_4)
isPFieldPun :: LHsFieldBind GhcPs (LFieldOcc GhcPs) (LPat GhcPs) -> Bool
#else
isPFieldPun :: LHsRecField GhcPs (LPat GhcPs) -> Bool
#endif
#if defined (GHC_9_10) || defined (GHC_9_8) || defined (GHC_9_6) || defined (GHC_9_4)
isPFieldPun (L _ HsFieldBind {hfbPun=True}) = True
#elif defined (GHCLIB_API_902) || defined (GHC_9_0) || defined (GHC_8_10)
isPFieldPun (L _ HsRecField {hsRecPun=True}) = True
#else
isPFieldPun (dL -> L _ HsRecField {hsRecPun=True}) = True
#endif
isPFieldPun _ = False

isPatTypeSig, isPBangPat, isPViewPat :: LPat GhcPs -> Bool
#if defined (GHC_9_10) || defined (GHC_9_8) || defined (GHC_9_6) || defined (GHC_9_4) || defined (GHCLIB_API_902) || defined (GHC_9_0) || defined (GHC_8_10)
isPatTypeSig (L _ SigPat{}) = True; isPatTypeSig _ = False
isPBangPat (L _ BangPat{}) = True; isPBangPat _ = False
isPViewPat (L _ ViewPat{}) = True; isPViewPat _ = False
#else
isPatTypeSig (dL -> L _ SigPat{}) = True; isPatTypeSig _ = False
isPBangPat (dL -> L _ BangPat{}) = True; isPBangPat _ = False
isPViewPat (dL -> L _ ViewPat{}) = True; isPViewPat _ = False
#endif

isSplicePat :: LPat GhcPs -> Bool
#if defined (GHC_9_10) || defined (GHC_9_8) || defined (GHC_9_6) || defined (GHC_9_4) || defined (GHCLIB_API_902) || defined (GHC_9_0) || defined (GHC_8_10)
isSplicePat (L _ SplicePat{}) = True; isSplicePat _ = False
#else
isSplicePat (dL -> L _ SplicePat{}) = True; isSplicePat _ = False
#endif
