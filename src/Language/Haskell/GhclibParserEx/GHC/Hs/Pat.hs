-- Copyright (c) 2020, Shayne Fletcher. All rights reserved.
-- SPDX-License-Identifier: BSD-3-Clause.

{-# LANGUAGE CPP #-}
{-# LANGUAGE ViewPatterns #-}
#include "ghclib_api.h"
module Language.Haskell.GhclibParserEx.GHC.Hs.Pat(
    patToStr
  , strToPat  ) where

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
