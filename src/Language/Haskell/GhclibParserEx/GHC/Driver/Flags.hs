{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -Wno-orphans #-}
#include "ghclib_api.h"
module Language.Haskell.GhclibParserEx.GHC.Driver.Flags () where

#if !(defined (GHCLIB_API_HEAD) || defined(GHCLIB_API_904) || defined(GHCLIB_API_902) || defined (GHCLIB_API_900))
import DynFlags
#endif

-- This instance landed in
-- https://gitlab.haskell.org/ghc/ghc/merge_requests/2905.
#if defined(GHCLIB_API_808) || defined(GHCLIB_API_810)
instance Bounded Language where
  minBound = Haskell98
  maxBound = Haskell2010
#endif
