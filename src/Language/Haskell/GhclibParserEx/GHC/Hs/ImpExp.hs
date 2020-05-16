-- Copyright (c) 2020, Shayne Fletcher. All rights reserved.
-- SPDX-License-Identifier: BSD-3-Clause.
{-# LANGUAGE CPP #-}
#include "ghclib_api.h"
module Language.Haskell.GhclibParserEx.GHC.Hs.ImpExp(
  isPatSynIE
  )
where

#if defined (GHCLIB_API_811)
import GHC.Hs.ImpExp
import GHC.Types.Name.Reader
#elif defined (GHCLIB_API_810)
import GHC.Hs.ImpExp
import RdrName
#else
import HsImpExp
import RdrName
#endif

isPatSynIE :: IEWrappedName RdrName -> Bool
isPatSynIE IEPattern{} = True
isPatSynIE _ = False
