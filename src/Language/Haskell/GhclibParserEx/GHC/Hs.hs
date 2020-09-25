-- Copyright (c) 2020, Shayne Fletcher. All rights reserved.
-- SPDX-License-Identifier: BSD-3-Clause.

{-# LANGUAGE CPP #-}
#include "ghclib_api.h"

module Language.Haskell.GhclibParserEx.GHC.Hs(
   modName
 )
where

#if defined(GHCLIB_API_901)
import GHC.Hs
import GHC.Unit.Module
import GHC.Types.SrcLoc
#elif defined(GHCLIB_API_810)
import GHC.Hs
import Module
import SrcLoc
#else
import HsSyn
import Module
import SrcLoc
#endif

#if defined(GHCLIB_API_901)
modName :: Located HsModule -> String
#else
modName :: Located (HsModule GhcPs) -> String
#endif
modName (L _ HsModule {hsmodName=Nothing}) = "Main"
modName (L _ HsModule {hsmodName=Just (L _ n)}) = moduleNameString n
