-- Copyright (c) 2020, Shayne Fletcher. All rights reserved.
-- SPDX-License-Identifier: BSD-3-Clause.

{-# LANGUAGE CPP #-}
#include "ghclib_api.h"
module Language.Haskell.GhclibParserEx.GHC.Hs.Decls(
    isNewType, isForD, isDerivD, isClsDefSig
  ) where

#if defined (GHCLIB_API_HEAD) || defined (GHCLIB_API_904) || defined(GHCLIB_API_902) || defined (GHCLIB_API_900) || defined (GHCLIB_API_810)
import GHC.Hs
#else
import HsSyn
#endif
#if defined (GHCLIB_API_HEAD) || defined (GHCLIB_API_904) || defined(GHCLIB_API_902) || defined (GHCLIB_API_900)
import GHC.Types.SrcLoc
#else
import SrcLoc
#endif

isNewType :: NewOrData -> Bool
isNewType NewType = True
isNewType DataType = False

isForD, isDerivD :: LHsDecl GhcPs -> Bool
isForD (L _ ForD{}) = True; isForD _ = False
isDerivD (L _ DerivD{}) = True; isDerivD _ = False

isClsDefSig :: Sig GhcPs -> Bool
isClsDefSig (ClassOpSig _ True _ _) = True; isClsDefSig _ = False
