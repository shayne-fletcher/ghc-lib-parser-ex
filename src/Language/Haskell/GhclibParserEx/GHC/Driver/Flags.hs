{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -Wno-orphans #-}
#include "ghclib_api.h"
module Language.Haskell.GhclibParserEx.GHC.Driver.Flags () where

#if defined(GHCLIB_API_811)
import GHC.Driver.Session
#else
import DynFlags
#endif

-- Oprhan instance until
-- https://gitlab.haskell.org/ghc/ghc/merge_requests/2905 lands.
instance Bounded Language where
  minBound = Haskell98
  maxBound = Haskell2010
