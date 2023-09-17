-- Copyright (c) 2020-2023 Shayne Fletcher. All rights reserved.
-- SPDX-License-Identifier: BSD-3-Clause.

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

#if defined (GHC_8_8)
import HsImpExp
import RdrName
#elif defined (GHC_8_10)
import GHC.Hs.ImpExp
import RdrName
#elif defined (GHC_9_0) || defined (GHC_9_2) || defined (GHC_9_4)
import GHC.Hs.ImpExp
import GHC.Types.Name.Reader
#else
import GHC.Hs.ImpExp
import GHC.Hs.Extension (GhcPs)
#endif

#if !( defined (GHC_9_4) || defined (GHC_9_2) || defined (GHC_9_0) || defined (GHC_8_10) || defined (GHC_8_8) )
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
