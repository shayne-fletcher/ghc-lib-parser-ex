-- Copyright (c) 2020, Shayne Fletcher. All rights reserved.
-- SPDX-License-Identifier: BSD-3-Clause.

{-# LANGUAGE CPP #-}
#include "ghclib_api.h"
module Language.Haskell.GhclibParserEx.GHC.Utils.Outputable (
    unsafePrettyPrint
)
where

#if defined (GHCLIB_API_HEAD) || defined (GHCLIB_API_904) || defined(GHCLIB_API_902) || defined (GHCLIB_API_900)
import GHC.Utils.Outputable
#else
import Outputable
#endif

unsafePrettyPrint :: Outputable a => a -> String
unsafePrettyPrint =
#if defined (GHCLIB_API_HEAD) || defined (GHCLIB_API_904)
  showPprUnsafe . ppr
#else
  showSDocUnsafe . ppr
#endif
