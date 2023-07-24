-- Copyright (c) 2020-2023, Shayne Fletcher. All rights reserved.
-- SPDX-License-Identifier: BSD-3-Clause.

{-# LANGUAGE CPP #-}
#include "ghclib_api.h"
module Language.Haskell.GhclibParserEx.GHC.Utils.Outputable (
    unsafePrettyPrint
)
where

#if defined (GHC_9_10) || defined (GHC_9_8) || defined (GHC_9_6) || defined (GHC_9_4) || defined(GHC_9_2) || defined (GHC_9_0)
import GHC.Utils.Outputable
#else
import Outputable
#endif

unsafePrettyPrint :: Outputable a => a -> String
unsafePrettyPrint =
#if defined (GHC_9_10) || defined (GHC_9_8) || defined (GHC_9_6) || defined (GHC_9_4)
  showPprUnsafe . ppr
#else
  showSDocUnsafe . ppr
#endif
