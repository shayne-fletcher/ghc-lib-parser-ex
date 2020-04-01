-- Copyright (c) 2020, Shayne Fletcher. All rights reserved.
-- SPDX-License-Identifier: BSD-3-Clause.

{-# OPTIONS_GHC -Wno-missing-fields #-}
{-# LANGUAGE CPP #-}
#include "ghclib_api.h"

module Language.Haskell.GhclibParserEx.Parse(
    parseFile
  , parseModule
  , parseSignature
  , parseImport
  , parseStatement
  , parseBackpack
  , parseDeclaration
  , parseExpression
  , parsePattern
  , parseTypeSignature
  , parseStmt
  , parseIdentifier
  , parseType
  , parseHeader
  , parse
  )
  where

#if defined (GHCLIB_API_811) || defined (GHCLIB_API_810)
import GHC.Hs
import RdrHsSyn
#else
import HsSyn
#endif
#if defined (GHCLIB_API_811)
import GHC.Driver.Session
#else
import DynFlags
#endif
import StringBuffer
import Lexer
import qualified Parser
import FastString
#if defined (GHCLIB_API_811)
import GHC.Types.SrcLoc
#else
import SrcLoc
#endif
#if defined (GHCLIB_API_811)
import GHC.Driver.Backpack.Syntax
#else
import BkpSyn
#endif
#if defined (GHCLIB_API_811)
import UnitInfo
#else
import PackageConfig
#endif
#if defined (GHCLIB_API_811)
import GHC.Types.Name.Reader
#else
import RdrName
#endif

parse :: P a -> String -> DynFlags -> ParseResult a
parse p str flags =
  Lexer.unP p parseState
  where
    location = mkRealSrcLoc (mkFastString "<string>") 1 1
    buffer = stringToStringBuffer str
    parseState = mkPState flags buffer location

#if defined (GHCLIB_API_811)
parseModule :: String -> DynFlags -> ParseResult (Located HsModule)
#else
parseModule :: String -> DynFlags -> ParseResult (Located (HsModule GhcPs))
#endif
parseModule = parse Parser.parseModule

#if defined (GHCLIB_API_811)
parseSignature :: String -> DynFlags -> ParseResult (Located HsModule)
#else
parseSignature :: String -> DynFlags -> ParseResult (Located (HsModule GhcPs))
#endif
parseSignature = parse Parser.parseSignature

parseImport :: String -> DynFlags -> ParseResult (LImportDecl GhcPs)
parseImport = parse Parser.parseImport

parseStatement :: String -> DynFlags -> ParseResult (LStmt GhcPs (LHsExpr GhcPs))
parseStatement = parse Parser.parseStatement

parseBackpack :: String -> DynFlags -> ParseResult [LHsUnit PackageName]
parseBackpack = parse Parser.parseBackpack

parseDeclaration :: String -> DynFlags -> ParseResult (LHsDecl GhcPs)
parseDeclaration = parse Parser.parseDeclaration

#if defined (GHCLIB_API_811) || defined (GHCLIB_API_810)
parseExpression :: String -> DynFlags -> ParseResult RdrHsSyn.ECP
#else
parseExpression :: String -> DynFlags -> ParseResult (LHsExpr GhcPs)
#endif
parseExpression = parse Parser.parseExpression

parsePattern :: String -> DynFlags -> ParseResult (LPat GhcPs)
parsePattern = parse Parser.parsePattern

parseTypeSignature :: String -> DynFlags -> ParseResult (LHsDecl GhcPs)
parseTypeSignature = parse Parser.parseTypeSignature

parseStmt :: String -> DynFlags -> ParseResult (Maybe (LStmt GhcPs (LHsExpr GhcPs)))
parseStmt = parse Parser.parseStmt

parseIdentifier :: String -> DynFlags -> ParseResult (Located RdrName)
parseIdentifier = parse Parser.parseIdentifier

parseType :: String -> DynFlags -> ParseResult (LHsType GhcPs)
parseType = parse Parser.parseType

#if defined(GHCLIB_API_811)
parseHeader :: String -> DynFlags -> ParseResult (Located HsModule)
#else
parseHeader :: String -> DynFlags -> ParseResult (Located (HsModule GhcPs))
#endif
parseHeader = parse Parser.parseHeader

#if defined (GHCLIB_API_811)
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
