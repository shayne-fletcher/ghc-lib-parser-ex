-- Copyright (c) 2020, Shayne Fletcher. All rights reserved.
-- SPDX-License-Identifier: BSD-3-Clause.

{-# LANGUAGE CPP #-}
#include "ghclib_api.h"

module Language.Haskell.GhclibParserEx.GHC.Types.Name.Reader(
   occNameStr, rdrNameStr, isSpecial, unqual, fromQual
 )
where

#if defined(GHCLIB_API_811)
import GHC.Types.SrcLoc
import GHC.Types.Name
import GHC.Types.Name.Reader
#else
import SrcLoc
import RdrName
import OccName
import Name
#endif

-- These names may not seem natural here but they work out in
-- practice. The use of thse two functions is thoroughly ubiquitous.
occNameStr :: RdrName -> String; occNameStr = occNameString . rdrNameOcc
rdrNameStr :: Located RdrName -> String; rdrNameStr = occNameStr . unLoc

-- Builtin type or data constructors.
isSpecial :: Located RdrName -> Bool
isSpecial (L _ (Exact n)) = isDataConName n || isTyConName n
isSpecial _ = False

-- Coerce qualified names to unqualified (by discarding the
-- qualifier).
unqual :: Located RdrName -> Located RdrName
unqual (L loc (Qual _ n)) = L loc $ mkRdrUnqual n
unqual x = x

fromQual :: Located RdrName -> Maybe OccName
fromQual (L _ (Qual _ x)) = Just x
fromQual (L _ (Unqual x)) = Just x
fromQual _ = Nothing
