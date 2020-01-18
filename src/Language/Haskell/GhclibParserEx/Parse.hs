-- Copyright (c) 2020, Shayne Fletcher. All rights reserved.
-- SPDX-License-Identifier: BSD-3-Clause.

{-# OPTIONS_GHC -Wno-missing-fields #-}
{-# LANGUAGE CPP #-}
#include "ghclib_api.h"

module Language.Haskell.GhclibParserEx.Parse(
    parse
  , parseExpr
  , parseImport
  , parseDeclaration
  , parseFile
  , parsePragmasIntoDynFlags
  , fakeSettings
  , fakeLlvmConfig
  )
  where

#if defined (GHCLIB_API_811) || defined (GHCLIB_API_810)
import GHC.Hs
import RdrHsSyn
#else
import HsSyn
#endif
import Config
import DynFlags
import StringBuffer
import Fingerprint
import Lexer
import qualified Parser
import FastString
import SrcLoc
import Panic
import HscTypes
import HeaderInfo

#if defined (GHCLIB_API_811) || defined (GHCLIB_API_810)
import GHC.Platform
import ToolSettings
#else
import Platform
#endif

fakeSettings :: Settings
fakeSettings = Settings
#if defined (GHCLIB_API_811) || defined (GHCLIB_API_810)
  { sGhcNameVersion=ghcNameVersion
  , sFileSettings=fileSettings
  , sTargetPlatform=platform
  , sPlatformMisc=platformMisc
  , sPlatformConstants=platformConstants
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
#if defined (GHCLIB_API_811) || defined (GHCLIB_API_810)
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
      Platform{
#if defined (GHCLIB_API_811) || defined (GHCLIB_API_810)
        platformWordSize = PW8
      , platformMini = PlatformMini {platformMini_arch=ArchUnknown, platformMini_os=OSUnknown}
#else
        platformWordSize=8
      , platformOS=OSUnknown
#endif
      , platformUnregisterised=True
      }
    platformConstants =
      PlatformConstants{pc_DYNAMIC_BY_DEFAULT=False,pc_WORD_SIZE=8}

#if defined (GHCLIB_API_811) || defined (GHCLIB_API_810)
fakeLlvmConfig :: LlvmConfig
fakeLlvmConfig = LlvmConfig [] []
#else
fakeLlvmConfig :: (LlvmTargets, LlvmPasses)
fakeLlvmConfig = ([], [])
#endif

parse :: P a -> String -> DynFlags -> ParseResult a
parse p str flags =
  Lexer.unP p parseState
  where
    location = mkRealSrcLoc (mkFastString "<string>") 1 1
    buffer = stringToStringBuffer str
    parseState = mkPState flags buffer location

#if defined (GHCLIB_API_811)
parseExpr :: String -> DynFlags -> ParseResult RdrHsSyn.ECP
#else
parseExpr :: String -> DynFlags -> ParseResult (LHsExpr GhcPs)
#endif
parseExpr = parse Parser.parseExpression

parseImport :: String -> DynFlags -> ParseResult (LImportDecl GhcPs)
parseImport = parse Parser.parseImport

parseDeclaration :: String -> DynFlags -> ParseResult (LHsDecl GhcPs)
parseDeclaration = parse Parser.parseDeclaration

#if defined (GHC_API_811)
parseFile :: String
          -> DynFlags
          -> String
          -> ParseResult (Located HsModule)
#else
parseFile :: String
          -> DynFlags
          -> String
          -> ParseResult (Located (HsModule GhcPs))
#endif
parseFile filename flags str =
  unP Parser.parseModule parseState
  where
    location = mkRealSrcLoc (mkFastString filename) 1 1
    buffer = stringToStringBuffer str
    parseState = mkPState flags buffer location

parsePragmasIntoDynFlags :: DynFlags
                         -> FilePath
                         -> String
                         -> IO (Either String DynFlags)
parsePragmasIntoDynFlags flags file str =
  catchErrors $ do
    let opts = getOptions flags (stringToStringBuffer str) file
    (flags, _, _) <- parseDynamicFilePragma flags opts
    return $ Right (flags `gopt_set` Opt_KeepRawTokenStream)
  where
    catchErrors :: IO (Either String DynFlags) -> IO (Either String DynFlags)
    catchErrors act = handleGhcException reportErr
                        (handleSourceError reportErr act)
    reportErr e = return $ Left (show e)
