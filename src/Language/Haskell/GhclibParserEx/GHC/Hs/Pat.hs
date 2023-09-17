-- Copyright (c) 2020-203, Shayne Fletcher. All rights reserved.
-- SPDX-License-Identifier: BSD-3-Clause.

{-# LANGUAGE ViewPatterns #-}
#include "ghclib_api.h"
module Language.Haskell.GhclibParserEx.GHC.Hs.Pat(
    patToStr, strToPat
  , fromPChar
  , hasPFieldsDotDot
  , isPFieldWildcard, isPWildcard, isPFieldPun, isPatTypeSig, isPBangPat, isPViewPat
  , isSplicePat
 ) where

#if defined (GHC_8_8)
import HsSyn
import SrcLoc
import TysWiredIn
import RdrName
import OccName
import FastString
#elif defined (GHC_8_10)
import GHC.Hs
import SrcLoc
import TysWiredIn
import RdrName
import OccName
import FastString
#else
import GHC.Hs
import GHC.Types.SrcLoc
import GHC.Builtin.Types
import GHC.Types.Name.Reader
import GHC.Types.Name
import GHC.Data.FastString
#endif

patToStr :: LPat GhcPs -> String
#if defined (GHC_8_8)
patToStr (dL -> L _ (ConPatIn (L _ x) (PrefixCon []))) | occNameString (rdrNameOcc x) == "True" = "True"
patToStr (dL -> L _ (ConPatIn (L _ x) (PrefixCon []))) | occNameString (rdrNameOcc x) == "False" = "False"
patToStr (dL -> L _ (ConPatIn (L _ x) (PrefixCon []))) | occNameString (rdrNameOcc x) == "[]" = "[]"
patToStr _ = ""
#elif defined (GHC_8_10)
patToStr (L _ (ConPatIn (L _ x) (PrefixCon []))) | occNameString (rdrNameOcc x) == "True" = "True"
patToStr (L _ (ConPatIn (L _ x) (PrefixCon []))) | occNameString (rdrNameOcc x) == "False" = "False"
patToStr (L _ (ConPatIn (L _ x) (PrefixCon []))) | occNameString (rdrNameOcc x) == "[]" = "[]"
patToStr _ = ""
#elif defined (GHC_9_0)
patToStr (L _ (ConPat _ (L _ x) (PrefixCon []))) | occNameString (rdrNameOcc x) == "True" = "True"
patToStr (L _ (ConPat _ (L _ x) (PrefixCon []))) | occNameString (rdrNameOcc x) == "False" = "False"
patToStr (L _ (ConPat _ (L _ x) (PrefixCon []))) | occNameString (rdrNameOcc x) == "[]" = "[]"
patToStr _ = ""
#else
patToStr (L _ (ConPat _ (L _ x) (PrefixCon [] []))) | occNameString (rdrNameOcc x) == "True" = "True"
patToStr (L _ (ConPat _ (L _ x) (PrefixCon [] []))) | occNameString (rdrNameOcc x) == "False" = "False"
patToStr (L _ (ConPat _ (L _ x) (PrefixCon [] []))) | occNameString (rdrNameOcc x) == "[]" = "[]"
patToStr _ = ""
#endif

strToPat :: String -> LPat GhcPs
strToPat z
#if defined (GHC_8_8)
  | z == "True" = ConPatIn (noLoc true_RDR) (PrefixCon [])
  | z == "False" = ConPatIn (noLoc false_RDR) (PrefixCon [])
  | z == "[]" = ConPatIn (noLoc $ nameRdrName nilDataConName) (PrefixCon [])
  | otherwise = VarPat noExt (noLoc $ mkVarUnqual (fsLit z))
#elif defined (GHC_8_10)
  | z == "True" =  noLoc $ ConPatIn (noLoc true_RDR) (PrefixCon [])
  | z == "False" =  noLoc $ ConPatIn (noLoc false_RDR) (PrefixCon [])
  | z == "[]" = noLoc $ ConPatIn (noLoc $ nameRdrName nilDataConName) (PrefixCon [])
  | otherwise = noLoc $ VarPat noExtField (noLoc $ mkVarUnqual (fsLit z))
#elif defined (GHC_9_0)
  | z == "True" = noLoc $ ConPat noExtField (noLoc true_RDR) (PrefixCon [])
  | z == "False" = noLoc $ ConPat noExtField (noLoc false_RDR) (PrefixCon [])
  | z == "[]" = noLoc $ ConPat noExtField (noLoc $ nameRdrName nilDataConName) (PrefixCon [])
  | otherwise = noLoc $ VarPat noExtField (noLoc $ mkVarUnqual (fsLit z))
#else
  | z == "True" = noLocA $ ConPat noAnn (noLocA true_RDR) (PrefixCon [] [])
  | z == "False" = noLocA $ ConPat noAnn (noLocA false_RDR) (PrefixCon [] [])
  | z == "[]" = noLocA $ ConPat noAnn (noLocA $ nameRdrName nilDataConName) (PrefixCon [] [])
  | otherwise = noLocA $ VarPat noExtField (noLocA $ mkVarUnqual (fsLit z))
#endif

fromPChar :: LPat GhcPs -> Maybe Char
#if defined (GHC_8_8)
fromPChar (dL -> L _ (LitPat _ (HsChar _ x))) = Just x
#else
fromPChar (L _ (LitPat _ (HsChar _ x))) = Just x
#endif
fromPChar _ = Nothing

-- Contains a '..' as in 'Foo{..}'
hasPFieldsDotDot :: HsRecFields GhcPs (LPat GhcPs) -> Bool
hasPFieldsDotDot HsRecFields {rec_dotdot=Just _} = True
hasPFieldsDotDot _ = False

-- Field has a '_' as in '{foo=_} or is punned e.g. '{foo}'.
#if !( defined (GHC_9_2) || defined (GHC_9_0) || defined (GHC_9_0) || defined (GHC_8_10) || defined (GHC_8_8) )
-- ghc >= 9.4
isPFieldWildcard :: LHsFieldBind GhcPs (LFieldOcc GhcPs) (LPat GhcPs) -> Bool
#else
isPFieldWildcard :: LHsRecField GhcPs (LPat GhcPs) -> Bool
#endif
#if defined (GHC_8_8)
isPFieldWildcard (dL -> L _ HsRecField {hsRecFieldArg=LL _ WildPat {}}) = True
isPFieldWildcard (dL -> L _ HsRecField {hsRecPun=True}) = True
isPFieldWildcard (dL -> L _ HsRecField {}) = False
#elif defined (GHC_8_10)
isPFieldWildcard (L _ HsRecField {hsRecFieldArg=L _ WildPat {}}) = True
isPFieldWildcard (L _ HsRecField {hsRecPun=True}) = True
isPFieldWildcard (L _ HsRecField {}) = False
#elif defined (GHC_9_0) || defined (GHC_9_2)
isPFieldWildcard (L _ HsRecField {hsRecFieldArg=L _ WildPat {}}) = True
isPFieldWildcard (L _ HsRecField {hsRecPun=True}) = True
isPFieldWildcard (L _ HsRecField {}) = False
#else
isPFieldWildcard (L _ HsFieldBind {hfbRHS=L _ WildPat {}}) = True
isPFieldWildcard (L _ HsFieldBind {hfbPun=True}) = True
isPFieldWildcard (L _ HsFieldBind {}) = False
#endif

isPWildcard :: LPat GhcPs -> Bool
#if defined (GHC_8_8)
isPWildcard (dL -> L _ (WildPat _)) = True
#else
isPWildcard (L _ (WildPat _)) = True
#endif
isPWildcard _ = False

#if !( defined (GHC_9_2) || defined (GHC_9_0) || defined (GHC_9_0) || defined (GHC_8_10) || defined (GHC_8_8) )
isPFieldPun :: LHsFieldBind GhcPs (LFieldOcc GhcPs) (LPat GhcPs) -> Bool
#else
isPFieldPun :: LHsRecField GhcPs (LPat GhcPs) -> Bool
#endif
#if defined (GHC_8_8)
isPFieldPun (dL -> L _ HsRecField {hsRecPun=True}) = True
#elif defined (GHC_8_10) || defined (GHC_9_0) || defined (GHC_9_2)
isPFieldPun (L _ HsRecField {hsRecPun=True}) = True
#else
isPFieldPun (L _ HsFieldBind {hfbPun=True}) = True
#endif
isPFieldPun _ = False

isPatTypeSig, isPBangPat, isPViewPat :: LPat GhcPs -> Bool
#if defined (GHC_8_8)
isPatTypeSig (dL -> L _ SigPat{}) = True; isPatTypeSig _ = False
isPBangPat (dL -> L _ BangPat{}) = True; isPBangPat _ = False
isPViewPat (dL -> L _ ViewPat{}) = True; isPViewPat _ = False
#else
isPatTypeSig (L _ SigPat{}) = True; isPatTypeSig _ = False
isPBangPat (L _ BangPat{}) = True; isPBangPat _ = False
isPViewPat (L _ ViewPat{}) = True; isPViewPat _ = False
#endif

isSplicePat :: LPat GhcPs -> Bool
#if defined (GHC_8_8)
isSplicePat (dL -> L _ SplicePat{}) = True; isSplicePat _ = False
#else
isSplicePat (L _ SplicePat{}) = True; isSplicePat _ = False
#endif
