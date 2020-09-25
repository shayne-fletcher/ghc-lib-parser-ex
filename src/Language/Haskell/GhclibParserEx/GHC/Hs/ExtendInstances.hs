-- Copyright (c) 2020, Shayne Fletcher. All rights reserved.
-- SPDX-License-Identifier: BSD-3-Clause.

{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE CPP #-}
#include "ghclib_api.h"

module Language.Haskell.GhclibParserEx.GHC.Hs.ExtendInstances (
    HsExtendInstances(..), extendInstances, astEq, astListEq)
where

-- At times, there are terms in Haskell syntax we work with that are
-- not in `Eq`, `Show` or `Ord` and we need them to be.

-- This work-around resorts to implementing Eq and Ord via
-- lexicographic string comparisons. As long as two different terms
-- never map to the same string, basing `Eq` and `Ord` on their string
-- representations rather than the terms themselves, leads to
-- identical results.

#if defined (GHCLIB_API_901)
import GHC.Utils.Outputable
import GHC.Driver.Ppr
#else
import Outputable
#endif
import Data.Data
import Data.Function
import Language.Haskell.GhclibParserEx.Dump

newtype HsExtendInstances a =
  HsExtendInstances { unextendInstances :: a }
    deriving Outputable

extendInstances :: a -> HsExtendInstances a
extendInstances = HsExtendInstances

-- Use 'showAstData'. This is preferable to 'ppr' in that trees that
-- only differ in arrangement due to fixities will produce differing
-- string representations.
toStr :: Data a => HsExtendInstances a -> String
toStr (HsExtendInstances e) =
#if defined(GHCLIB_API_901)
  showPprUnsafe $ showAstData BlankSrcSpan e
#else
  showSDocUnsafe $ showAstData BlankSrcSpan e
#endif

instance Data a => Eq (HsExtendInstances a) where (==) a b = toStr a == toStr b
instance Data a => Ord (HsExtendInstances a) where compare = compare `on` toStr

astEq :: Data a => a -> a -> Bool
astEq a b = extendInstances a == extendInstances b

astListEq :: Data a => [a] -> [a] -> Bool
astListEq as bs = length as == length bs && all (uncurry astEq) (zip as bs)

-- Use 'ppr' for 'Show'.
instance Outputable a => Show (HsExtendInstances a) where
  show (HsExtendInstances e) =
#if defined(GHCLIB_API_901)
    showPprUnsafe $ ppr e
#else
    showSDocUnsafe $ ppr e
#endif
