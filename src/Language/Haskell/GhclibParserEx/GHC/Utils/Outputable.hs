-- Copyright (c) 2020, Shayne Fletcher. All rights reserved.
-- SPDX-License-Identifier: BSD-3-Clause.

{-# LANGUAGE CPP #-}
#include "ghclib_api.h"
module Language.Haskell.GhclibParserEx.GHC.Utils.Outputable (
    unsafePrettyPrint
)
where

#if defined (GHCLIB_API_HEAD) || defined(GHCLIB_API_902) || defined (GHCLIB_API_900)
import GHC.Utils.Outputable
#if !defined(GHCLIB_API_HEAD) && !defined (GHCLIB_API_900)
import GHC.Driver.Ppr
#endif
#else
import Outputable
#endif

unsafePrettyPrint :: Outputable a => a -> String
unsafePrettyPrint =
#if defined (GHCLIB_API_HEAD)
  showPprUnsafe . ppr
#else
  showSDocUnsafe . ppr
#endif
