-- Copyright (c) 2020, Shayne Fletcher. All rights reserved.
-- SPDX-License-Identifier: BSD-3-Clause.
{-# LANGUAGE CPP #-}
#include "ghclib_api.h"

module Compat.GHC.Hs (
#if defined (GHCLIB_API_811) || defined (GHCLIB_API_810)
  module GHC.Hs
#else
  module HsSyn
#endif
) where

#if defined (GHCLIB_API_811) || defined (GHCLIB_API_810)
import GHC.Hs
#else
import HsSyn
#endif
