-- Copyright (c) 2020, Shayne Fletcher. All rights reserved.
-- SPDX-License-Identifier: BSD-3-Clause.

{-# LANGUAGE CPP #-}
#include "ghclib_api.h"
module Language.Haskell.GhclibParserEx.GHC.Hs(
   modName
 )
where

#if defined (GHC_9_10) || defined (GHC_9_8) || defined (GHC_9_6) || defined (GHC_9_4) || defined (GHC_9_2) || defined (GHC_9_0)
import GHC.Hs
#  if !defined (GHC_9_10) && !defined (GHC_9_8) && !defined (GHC_9_6)
import GHC.Unit.Module
#  endif
import GHC.Types.SrcLoc
#elif defined (GHC_8_10)
import GHC.Hs
import Module
import SrcLoc
#else
import HsSyn
import Module
import SrcLoc
#endif

#if defined (GHC_9_4) || defined (GHC_9_2) || defined (GHC_9_0)
modName :: Located HsModule -> String
#else
modName :: Located (HsModule GhcPs) -> String
#endif
modName (L _ HsModule {hsmodName=Nothing}) = "Main"
modName (L _ HsModule {hsmodName=Just (L _ n)}) = moduleNameString n
