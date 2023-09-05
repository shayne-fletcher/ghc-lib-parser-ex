-- Copyright (c) 2021-2023, Shayne Fletcher. All rights reserved.
-- SPDX-License-Identifier: BSD-3-Clause.

{-# LANGUAGE LambdaCase #-}
#include "ghclib_api.h"
module Language.Haskell.GhclibParserEx.GHC.Hs.Type (
    fromTyParen
  , isTyQuasiQuote, isUnboxedTuple, isKindTyApp
) where

#if defined (GHC_8_8)
import HsSyn
#else
import GHC.Hs
#endif
#if defined (GHC_8_8) || defined (GHC_8_10)
import SrcLoc
#else
import GHC.Types.SrcLoc
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
