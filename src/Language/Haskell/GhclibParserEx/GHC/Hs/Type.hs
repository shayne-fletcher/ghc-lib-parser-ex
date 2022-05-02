-- Copyright (c) 2021, Shayne Fletcher. All rights reserved.
-- SPDX-License-Identifier: BSD-3-Clause.

{-# LANGUAGE CPP #-}
{-# LANGUAGE LambdaCase #-}
#include "ghclib_api.h"
module Language.Haskell.GhclibParserEx.GHC.Hs.Type (
    fromTyParen
  , isTyQuasiQuote, isUnboxedTuple, isKindTyApp
) where

#if defined (GHCLIB_API_HEAD) || defined (GHCLIB_API_904) || defined (GHCLIB_API_902) || defined (GHCLIB_API_900) || defined (GHCLIB_API_810)
import GHC.Hs
#else
import HsSyn
#endif
#if defined (GHCLIB_API_HEAD) || defined (GHCLIB_API_904) || defined (GHCLIB_API_902) || defined (GHCLIB_API_900)
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
