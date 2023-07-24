-- Copyright (c) 2020-2023, Shayne Fletcher. All rights reserved.
-- SPDX-License-Identifier: BSD-3-Clause.

{-# OPTIONS_GHC -Wno-missing-fields #-}
{-# LANGUAGE CPP #-}
#include "ghclib_api.h"

module Language.Haskell.GhclibParserEx.GHC.Settings.Config(
    fakeSettings
#if !defined (GHC_9_10) && !defined (GHC_9_8) && !defined (GHC_9_6)
  , fakeLlvmConfig
#endif
  )
where

#if defined (GHC_9_10) || defined (GHC_9_8) || defined (GHC_9_6) || defined (GHC_9_4) || defined (GHCLIB_API_902) || defined (GHC_9_0)
import GHC.Settings.Config
import GHC.Driver.Session
import GHC.Utils.Fingerprint
import GHC.Platform
import GHC.Settings
#elif defined (GHC_8_10)
import Config
import DynFlags
import Fingerprint
import GHC.Platform
import ToolSettings
#else
import Config
import DynFlags
import Fingerprint
import Platform
#endif

fakeSettings :: Settings
fakeSettings = Settings
#if defined (GHC_9_10) || defined (GHC_9_8) || defined (GHC_9_6) || defined (GHC_9_4) || defined(GHCLIB_API_902) || defined (GHC_9_0)|| defined (GHC_8_10)
  { sGhcNameVersion=ghcNameVersion
  , sFileSettings=fileSettings
  , sTargetPlatform=platform
  , sPlatformMisc=platformMisc
#  if !defined(GHC_9_10) && !defined (GHC_9_8) && !defined(GHC_9_6) && !defined(GHC_9_4) && !defined(GHCLIB_API_902)
  , sPlatformConstants=platformConstants
#  endif
  , sToolSettings=toolSettings
  }
#else
  { sTargetPlatform=platform
  , sPlatformConstants=platformConstants
  , sProjectVersion=cProjectVersion
  , sProgramName="ghc"
  , sOpt_P_fingerprint=fingerprint0
  }
#endif
  where
#if defined (GHC_9_10) || defined (GHC_9_8) || defined (GHC_9_6) || defined (GHC_9_4) || defined(GHCLIB_API_902) || defined (GHC_9_0)|| defined (GHC_8_10)
    toolSettings = ToolSettings {
      toolSettings_opt_P_fingerprint=fingerprint0
      }
    fileSettings = FileSettings {}
    platformMisc = PlatformMisc {}
    ghcNameVersion =
      GhcNameVersion{ghcNameVersion_programName="ghc"
                    ,ghcNameVersion_projectVersion=cProjectVersion
                    }
#endif
    platform =
#if defined (GHC_9_10) || defined (GHC_9_8) || defined (GHC_9_6) || defined (GHC_9_4) || defined (GHCLIB_API_902)
      genericPlatform
#else
      Platform{
#  if defined (GHC_9_0)
    -- It doesn't matter what values we write here as these fields are
    -- not referenced for our purposes. However the fields are strict
    -- so we must say something.
        platformByteOrder=LittleEndian
      , platformHasGnuNonexecStack=True
      , platformHasIdentDirective=False
      , platformHasSubsectionsViaSymbols=False
      , platformIsCrossCompiling=False
      , platformLeadingUnderscore=False
      , platformTablesNextToCode=False
      ,
#  endif
#  if defined (GHC_8_10) || defined (GHC_9_0)
        platformWordSize=PW8
      , platformMini=PlatformMini {platformMini_arch=ArchUnknown, platformMini_os=OSUnknown}
#  else
        platformWordSize=8
      , platformOS=OSUnknown
#  endif
      , platformUnregisterised=True
      }
#endif
#if !defined(GHC_9_10) && !defined (GHC_9_8) && !defined(GHC_9_6) && !defined(GHC_9_4) && !defined(GHCLIB_API_902)
    platformConstants =
      PlatformConstants{pc_DYNAMIC_BY_DEFAULT=False,pc_WORD_SIZE=8}
#endif

#if defined (GHC_9_10) || defined (GHC_9_8) || defined (GHC_9_6)
-- Intentionally empty
#elif defined(GHC_9_4) || defined(GHCLIB_API_902) || defined (GHC_9_0)|| defined (GHC_8_10)
fakeLlvmConfig :: LlvmConfig
fakeLlvmConfig = LlvmConfig [] []
#else
fakeLlvmConfig :: (LlvmTargets, LlvmPasses)
fakeLlvmConfig = ([], [])
#endif
