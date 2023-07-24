-- Copyright (c) 2020-203, Shayne Fletcher. All rights reserved.
-- SPDX-License-Identifier: BSD-3-Clause.
{-# LANGUAGE CPP #-}
#include "ghclib_api.h"
module Language.Haskell.GhclibParserEx.GHC.Hs.Binds(
  isPatSynBind
  )
where

#if defined (GHC_9_10) || defined (GHC_9_8) || defined (GHC_9_6) || defined (GHC_9_4) || defined(GHCLIB_API_902) || defined (GHC_9_0) || defined (GHC_8_10)
import GHC.Hs.Binds
import GHC.Hs.Extension
#else
import HsBinds
import HsExtension
#endif

isPatSynBind :: HsBind GhcPs -> Bool
isPatSynBind PatSynBind{} = True
isPatSynBind _ = False
