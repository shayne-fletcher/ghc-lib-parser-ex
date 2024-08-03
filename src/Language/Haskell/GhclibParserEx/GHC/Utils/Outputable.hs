-- Copyright (c) 2020-2023, Shayne Fletcher. All rights reserved.
-- SPDX-License-Identifier: BSD-3-Clause.

{- ORMOLU_DISABLE -}
#include "ghclib_api.h"
module Language.Haskell.GhclibParserEx.GHC.Utils.Outputable (
    unsafePrettyPrint
)
where

#if defined (GHC_8_8) || defined (GHC_8_10)
import Outputable
#else
import GHC.Utils.Outputable
#endif

unsafePrettyPrint :: Outputable a => a -> String
unsafePrettyPrint =
#if ! ( defined (GHC_9_2) || defined (GHC_9_0) || defined (GHC_8_10) || defined (GHC_8_8) )
  showPprUnsafe . ppr
#else
  showSDocUnsafe . ppr
#endif
