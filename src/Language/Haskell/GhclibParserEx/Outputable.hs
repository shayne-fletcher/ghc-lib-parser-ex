-- Copyright (c) 2020, Shayne Fletcher. All rights reserved.
-- SPDX-License-Identifier: BSD-3-Clause.

module Language.Haskell.GhclibParserEx.Outputable (unsafePrettyPrint) where

import Outputable

unsafePrettyPrint :: (Outputable.Outputable a) => a -> String
unsafePrettyPrint = Outputable.showSDocUnsafe . Outputable.ppr
