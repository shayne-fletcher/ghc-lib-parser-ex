-- Copyright (c) 2020-2023, Shayne Fletcher. All rights reserved.
-- SPDX-License-Identifier: BSD-3-Clause.

#include "ghclib_api.h"
module Language.Haskell.GhclibParserEx.GHC.Hs
  ( modName,
  )
where

#if defined (GHC_8_8)
import HsSyn
import Module
import SrcLoc
#elif defined (GHC_8_10)
import GHC.Hs
import Module
import SrcLoc
#elif defined (GHC_9_0) || defined (GHC_9_2) || defined (GHC_9_4)
import GHC.Hs
import GHC.Unit.Module
import GHC.Types.SrcLoc
#else
import GHC.Hs
import GHC.Types.SrcLoc
#endif

#if defined (GHC_9_4) || defined (GHC_9_2) || defined (GHC_9_0)
modName :: Located HsModule -> String
#else
modName :: Located (HsModule GhcPs) -> String
#endif
modName (L _ HsModule {hsmodName = Nothing}) = "Main"
modName (L _ HsModule {hsmodName = Just (L _ n)}) = moduleNameString n
