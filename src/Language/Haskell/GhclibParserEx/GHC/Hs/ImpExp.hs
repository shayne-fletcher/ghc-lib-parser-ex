-- Copyright (c) 2020-2023 Shayne Fletcher. All rights reserved.
-- SPDX-License-Identifier: BSD-3-Clause.
{-# LANGUAGE CPP #-}
#include "ghclib_api.h"
module Language.Haskell.GhclibParserEx.GHC.Hs.ImpExp(
    isPatSynIE
#if defined (MIN_VERSION_ghc_lib_parser)
#  if !MIN_VERSION_ghc_lib_parser(1,  0,  0) || MIN_VERSION_ghc_lib_parser(8, 10, 0)
 , isImportQualifiedPost
#  endif
#elif  __GLASGOW_HASKELL__ >= 810
 , isImportQualifiedPost
#endif
  )
where

#if defined (GHC_9_10) || defined (GHC_9_8) || defined (GHC_9_6)
import GHC.Hs.Extension (GhcPs)
#endif

#if defined (GHC_9_10) || defined (GHC_9_8) || defined (GHC_9_6) || defined (GHC_9_4) || defined(GHC_9_2) || defined (GHC_9_0)
import GHC.Hs.ImpExp
#  if !defined (GHC_9_10) && !defined (GHC_9_8) && !defined (GHC_9_6)
import GHC.Types.Name.Reader
#  endif
#elif defined (GHC_8_10)
import GHC.Hs.ImpExp
import RdrName
#else
import HsImpExp
import RdrName
#endif

#if defined (GHC_9_10) || defined (GHC_9_8) || defined (GHC_9_6)
isPatSynIE :: IEWrappedName GhcPs -> Bool
#else
isPatSynIE :: IEWrappedName RdrName -> Bool
#endif
isPatSynIE IEPattern{} = True
isPatSynIE _ = False

#if defined (MIN_VERSION_ghc_lib_parser)
#  if !MIN_VERSION_ghc_lib_parser(1,  0,  0) || MIN_VERSION_ghc_lib_parser(8, 10, 0)
isImportQualifiedPost :: ImportDeclQualifiedStyle -> Bool
isImportQualifiedPost QualifiedPost = True
isImportQualifiedPost _  = False
#  endif
#elif __GLASGOW_HASKELL__ >= 810
isImportQualifiedPost :: ImportDeclQualifiedStyle -> Bool
isImportQualifiedPost QualifiedPost = True
isImportQualifiedPost _  = False
#endif
