-- Copyright (c) 2020-2023, Shayne Fletcher. All rights reserved.
-- SPDX-License-Identifier: BSD-3-Clause.

#include "ghclib_api.h"
module Language.Haskell.GhclibParserEx.GHC.Hs.Binds
  ( isPatSynBind,
  )
where

#if defined (GHC_8_8)
import HsBinds
import HsExtension
#else
import GHC.Hs.Binds
import GHC.Hs.Extension
#endif

isPatSynBind :: HsBind GhcPs -> Bool
isPatSynBind PatSynBind {} = True
isPatSynBind _ = False
