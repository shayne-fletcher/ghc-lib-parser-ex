-- Copyright (c) 2020-2023, Shayne Fletcher. All rights reserved.
-- SPDX-License-Identifier: BSD-3-Clause.

{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -Wno-orphans #-}
#include "ghclib_api.h"
module Language.Haskell.GhclibParserEx.GHC.Driver.Flags () where

#if ! (defined (GHC_9_10) || defined (GHC_9_8) || defined(GHC_9_6) || defined(GHC_9_4)  || defined(GHC_9_2) || defined (GHC_9_0))
import DynFlags
#endif

-- This instance landed in
-- https://gitlab.haskell.org/ghc/ghc/merge_requests/2905.
#if defined (GHC_8_8) || defined(GHC_8_10)
instance Bounded Language where
  minBound = Haskell98
  maxBound = Haskell2010
#endif
