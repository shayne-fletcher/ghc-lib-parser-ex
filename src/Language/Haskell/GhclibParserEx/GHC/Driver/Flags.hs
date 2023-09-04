-- Copyright (c) 2020-2023, Shayne Fletcher. All rights reserved.
-- SPDX-License-Identifier: BSD-3-Clause.

{-# OPTIONS_GHC -Wno-orphans #-}
#include "ghclib_api.h"
module Language.Haskell.GhclibParserEx.GHC.Driver.Flags () where

#if defined (GHC_8_10) || defined (GHC_8_8)
import DynFlags

-- This instance landed in
-- https://gitlab.haskell.org/ghc/ghc/merge_requests/2905.
instance Bounded Language where
  minBound = Haskell98
  maxBound = Haskell2010
#endif
