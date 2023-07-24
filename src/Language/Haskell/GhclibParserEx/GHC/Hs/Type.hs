-- Copyright (c) 2021-2023, Shayne Fletcher. All rights reserved.
-- SPDX-License-Identifier: BSD-3-Clause.

{-# LANGUAGE CPP #-}
{-# LANGUAGE LambdaCase #-}
#include "ghclib_api.h"
module Language.Haskell.GhclibParserEx.GHC.Hs.Type (
    fromTyParen
  , isTyQuasiQuote, isUnboxedTuple, isKindTyApp
) where

#if defined (GHC_9_10) || defined (GHC_9_8) || defined (GHC_9_6) || defined (GHC_9_4) || defined (GHCLIB_API_902) || defined (GHC_9_0) || defined (GHC_8_10)
import GHC.Hs
#else
import HsSyn
#endif
#if defined (GHC_9_10) || defined (GHC_9_8) || defined (GHC_9_6) || defined (GHC_9_4) || defined (GHCLIB_API_902) || defined (GHC_9_0)
import GHC.Types.SrcLoc
#else
import SrcLoc
#endif

isKindTyApp :: LHsType GhcPs -> Bool
isKindTyApp = \case (L _ HsAppKindTy{}) -> True; _ -> False

fromTyParen :: LHsType GhcPs -> LHsType GhcPs
fromTyParen (L _ (HsParTy _ x)) = x
fromTyParen x = x

isTyQuasiQuote :: LHsType GhcPs -> Bool
isTyQuasiQuote (L _ (HsSpliceTy _ HsQuasiQuote{})) = True
isTyQuasiQuote _ = False

isUnboxedTuple :: HsTupleSort -> Bool
isUnboxedTuple HsUnboxedTuple = True
isUnboxedTuple _ = False
