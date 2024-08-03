-- Copyright (c) 2020-2024, Shayne Fletcher. All rights reserved.
-- SPDX-License-Identifier: BSD-3-Clause.

{- ORMOLU_DISABLE -}
{-# OPTIONS_GHC -Wno-missing-fields #-}
#include "ghclib_api.h"
module Language.Haskell.GhclibParserEx.GHC.Settings.Config(
    fakeSettings
#if defined (GHC_8_8) || defined (GHC_8_10) || defined (GHC_9_0) || defined (GHC_9_2) || defined (GHC_9_4)
  , fakeLlvmConfig
#endif
  )
where

#if defined (GHC_8_8)
import Config
import DynFlags
import Fingerprint
import Platform
#elif defined (GHC_8_10)
import Config
import DynFlags
import Fingerprint
import GHC.Platform
import ToolSettings
#else
import GHC.Settings.Config
import GHC.Driver.Session
import GHC.Utils.Fingerprint
import GHC.Platform
import GHC.Settings
#endif

fakeSettings :: Settings
fakeSettings = Settings
#if defined (GHC_8_8)
  { sTargetPlatform=platform
  , sPlatformConstants=platformConstants
  , sProjectVersion=cProjectVersion
  , sProgramName="ghc"
  , sOpt_P_fingerprint=fingerprint0
  }
#elif defined (GHC_8_10) || defined (GHC_9_0)
  { sGhcNameVersion=ghcNameVersion
  , sFileSettings=fileSettings
  , sTargetPlatform=platform
  , sPlatformMisc=platformMisc
  , sPlatformConstants=platformConstants
  , sToolSettings=toolSettings
  }
#else
  { sGhcNameVersion=ghcNameVersion
  , sFileSettings=fileSettings
  , sTargetPlatform=platform
  , sPlatformMisc=platformMisc
  , sToolSettings=toolSettings
  }
#endif
  where
#if !defined (GHC_8_8)
    toolSettings = ToolSettings {
        toolSettings_opt_P_fingerprint=fingerprint0
      }
    fileSettings = FileSettings {}
    platformMisc = PlatformMisc {}
    ghcNameVersion = GhcNameVersion {
        ghcNameVersion_programName="ghc"
      , ghcNameVersion_projectVersion=cProjectVersion
      }
#endif
#if defined (GHC_8_8) || defined (GHC_8_10) || defined (GHC_9_0)
    platformConstants = PlatformConstants {
        pc_DYNAMIC_BY_DEFAULT=False
      , pc_WORD_SIZE=8
      }
#endif
#if defined (GHC_8_8)
    platform = Platform {
        platformWordSize=8
      , platformOS=OSUnknown
      , platformUnregisterised=True
      }
#elif defined (GHC_8_10)
    platform = Platform {
        platformWordSize=PW8
      , platformMini=PlatformMini {
            platformMini_arch=ArchUnknown
          , platformMini_os=OSUnknown
          }
      , platformUnregisterised=True
      }
#elif defined (GHC_9_0)
    platform = Platform {
        platformByteOrder=LittleEndian
      , platformHasGnuNonexecStack=True
      , platformHasIdentDirective=False
      , platformHasSubsectionsViaSymbols=False
      , platformIsCrossCompiling=False
      , platformLeadingUnderscore=False
      , platformTablesNextToCode=False
      , platformWordSize=PW8
      , platformMini=PlatformMini {platformMini_arch=ArchUnknown, platformMini_os=OSUnknown}
      , platformUnregisterised=True
      }
#else
    platform = genericPlatform
#endif

#if defined (GHC_8_8)
fakeLlvmConfig :: (LlvmTargets, LlvmPasses)
fakeLlvmConfig = ([], [])
#elif defined (GHC_8_10) || defined (GHC_9_0) || defined(GHC_9_2) || defined(GHC_9_4)
fakeLlvmConfig :: LlvmConfig
fakeLlvmConfig = LlvmConfig [] []
#endif
