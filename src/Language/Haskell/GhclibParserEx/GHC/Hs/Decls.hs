-- Copyright (c) 2020-2023, Shayne Fletcher. All rights reserved.
-- SPDX-License-Identifier: BSD-3-Clause.

#include "ghclib_api.h"
module Language.Haskell.GhclibParserEx.GHC.Hs.Decls
  ( isNewType,
    isForD,
    isDerivD,
    isClsDefSig,
  )
where

#if defined(GHC_8_8)
import HsSyn
#else
import GHC.Hs
#endif
#if defined (GHC_8_10) || defined (GHC_8_8)
import SrcLoc
#else
import GHC.Types.SrcLoc
#endif

isNewType :: NewOrData -> Bool
isNewType NewType = True
isNewType DataType = False

isForD, isDerivD :: LHsDecl GhcPs -> Bool
isForD (L _ ForD {}) = True; isForD _ = False
isDerivD (L _ DerivD {}) = True; isDerivD _ = False

isClsDefSig :: Sig GhcPs -> Bool
isClsDefSig (ClassOpSig _ True _ _) = True; isClsDefSig _ = False
