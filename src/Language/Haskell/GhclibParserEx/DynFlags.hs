-- Copyright (c) 2020, Shayne Fletcher. All rights reserved.
-- SPDX-License-Identifier: BSD-3-Clause.

module Language.Haskell.GhclibParserEx.DynFlags(
    parsePragmasIntoDynFlags
  ) where

import DynFlags
import Panic
import HeaderInfo
import StringBuffer
import HscTypes
import GHC.LanguageExtensions.Type
import Data.List

parsePragmasIntoDynFlags :: DynFlags
                         -> ([Extension], [Extension])
                         -> FilePath
                         -> String
                         -> IO (Either String DynFlags)
parsePragmasIntoDynFlags flags (enable, disable) file str =
  catchErrors $ do
    let opts = getOptions flags (stringToStringBuffer str) file
    (flags, _, _) <- parseDynamicFilePragma flags opts
    let flags' =  foldl' xopt_set flags enable
    let flags'' = foldl' xopt_unset flags' disable
    return $ Right (flags'' `gopt_set` Opt_KeepRawTokenStream)
  where
    catchErrors :: IO (Either String DynFlags) -> IO (Either String DynFlags)
    catchErrors act = handleGhcException reportErr
                        (handleSourceError reportErr act)
    reportErr e = return $ Left (show e)
