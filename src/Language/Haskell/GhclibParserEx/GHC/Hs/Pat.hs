-- Copyright (c) 2020, Shayne Fletcher. All rights reserved.
-- SPDX-License-Identifier: BSD-3-Clause.

{-# LANGUAGE CPP #-}
{-# LANGUAGE ViewPatterns #-}
#include "ghclib_api.h"
module Language.Haskell.GhclibParserEx.GHC.Hs.Pat(
    patToStr, strToPat
  , fromPChar
  , hasPFieldsDotDot
  , isPFieldWildcard, isPWildcard, isPFieldPun, isPatTypeSig, isPBangPat, isPViewPat
 ) where

#if defined (GHCLIB_API_811) || defined (GHCLIB_API_810)
import GHC.Hs
#else
import HsSyn
#endif
import SrcLoc
import TysWiredIn
import RdrName
import OccName
import FastString

patToStr :: LPat GhcPs -> String
#if defined (GHCLIB_API_811) || defined (GHCLIB_API_810)
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

strToPat :: String -> Pat GhcPs
strToPat z
  | z == "True"  = ConPatIn (noLoc true_RDR) (PrefixCon [])
  | z == "False" = ConPatIn (noLoc false_RDR) (PrefixCon [])
  | z == "[]"    = ConPatIn (noLoc $ nameRdrName nilDataConName) (PrefixCon [])
  | otherwise =
#if defined (GHCLIB_API_811) || defined (GHCLIB_API_810)
      VarPat noExtField (noLoc $ mkVarUnqual (fsLit z))
#else
      VarPat noExt (noLoc $ mkVarUnqual (fsLit z))
#endif

fromPChar :: LPat GhcPs -> Maybe Char
#if defined (GHCLIB_API_811) || defined (GHCLIB_API_810)
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
isPFieldWildcard :: LHsRecField GhcPs (Pat GhcPs) -> Bool
#if defined (GHCLIB_API_811) || defined (GHCLIB_API_810)
isPFieldWildcard (L _ HsRecField {hsRecFieldArg=WildPat _}) = True
isPFieldWildcard (L _ HsRecField {hsRecPun=True}) = True
isPFieldWildcard (L _ HsRecField {}) = False
#else
isPFieldWildcard (dL -> L _ HsRecField {hsRecFieldArg=WildPat _}) = True
isPFieldWildcard (dL -> L _ HsRecField {hsRecPun=True}) = True
isPFieldWildcard (dL -> L _ HsRecField {}) = False
#endif

isPWildcard :: LPat GhcPs -> Bool
#if defined (GHCLIB_API_811) || defined (GHCLIB_API_810)
isPWildcard (L _ (WildPat _)) = True
#else
isPWildcard (dL -> L _ (WildPat _)) = True
#endif
isPWildcard _ = False

isPFieldPun :: LHsRecField GhcPs (Pat GhcPs) -> Bool
#if defined (GHCLIB_API_811) || defined (GHCLIB_API_810)
isPFieldPun (L _ HsRecField {hsRecPun=True}) = True
#else
isPFieldPun (dL -> L _ HsRecField {hsRecPun=True}) = True
#endif
isPFieldPun _ = False

isPatTypeSig, isPBangPat, isPViewPat :: LPat GhcPs -> Bool
#if defined (GHCLIB_API_811) || defined (GHCLIB_API_810)
isPatTypeSig (L _ SigPat{}) = True; isPatTypeSig _ = False
isPBangPat (L _ BangPat{}) = True; isPBangPat _ = False
isPViewPat (L _ ViewPat{}) = True; isPViewPat _ = False
#else
isPatTypeSig (dL -> L _ SigPat{}) = True; isPatTypeSig _ = False
isPBangPat (dL -> L _ BangPat{}) = True; isPBangPat _ = False
isPViewPat (dL -> L _ ViewPat{}) = True; isPViewPat _ = False
#endif
