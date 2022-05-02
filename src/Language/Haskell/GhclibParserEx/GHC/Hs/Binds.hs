-- Copyright (c) 2020, Shayne Fletcher. All rights reserved.
-- SPDX-License-Identifier: BSD-3-Clause.
{-# LANGUAGE CPP #-}
#include "ghclib_api.h"
module Language.Haskell.GhclibParserEx.GHC.Hs.Binds(
  isPatSynBind
  )
where

#if defined (GHCLIB_API_HEAD) || defined (GHCLIB_API_904) || defined(GHCLIB_API_902) || defined (GHCLIB_API_900) || defined (GHCLIB_API_810)
import GHC.Hs.Binds
import GHC.Hs.Extension
#else
import HsBinds
import HsExtension
#endif

isPatSynBind :: HsBind GhcPs -> Bool
isPatSynBind PatSynBind{} = True
isPatSynBind _ = False
