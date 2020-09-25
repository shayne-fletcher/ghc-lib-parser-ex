-- Copyright (c) 2020, Shayne Fletcher. All rights reserved.
-- SPDX-License-Identifier: BSD-3-Clause.

{-# LANGUAGE CPP #-}
#include "ghclib_api.h"
module Language.Haskell.GhclibParserEx.GHC.Parser(
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

#if defined (GHCLIB_API_901) || defined (GHCLIB_API_810)
import GHC.Hs
#else
import HsSyn
#endif
#if defined (GHCLIB_API_901)
import GHC.Parser.PostProcess
import GHC.Driver.Session
import GHC.Data.StringBuffer
import GHC.Parser.Lexer
import qualified GHC.Parser.Lexer as Lexer
import qualified GHC.Parser as Parser
import GHC.Data.FastString
import GHC.Types.SrcLoc
import GHC.Driver.Backpack.Syntax
import GHC.Unit.Info
import GHC.Types.Name.Reader
#else
import DynFlags
import StringBuffer
import Lexer
import qualified Parser
import FastString
import SrcLoc
import BkpSyn
import PackageConfig
import RdrName
#endif
#if defined (GHCLIB_API_810)
import RdrHsSyn
#endif

parse :: P a -> String -> DynFlags -> ParseResult a
parse p str flags =
  Lexer.unP p parseState
  where
    location = mkRealSrcLoc (mkFastString "<string>") 1 1
    buffer = stringToStringBuffer str
    parseState = mkPState flags buffer location

#if defined (GHCLIB_API_901)
parseModule :: String -> DynFlags -> ParseResult (Located HsModule)
#else
parseModule :: String -> DynFlags -> ParseResult (Located (HsModule GhcPs))
#endif
parseModule = parse Parser.parseModule

#if defined (GHCLIB_API_901)
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

parseExpression :: String -> DynFlags -> ParseResult (LHsExpr GhcPs)
#if defined (GHCLIB_API_901)
parseExpression s flags =
  case parse Parser.parseExpression s flags of
    POk s e -> unP (runPV . unECP $ e) s
    PFailed ps -> PFailed ps
#elif defined (GHCLIB_API_810)
parseExpression s flags =
  case parse Parser.parseExpression s flags of
    POk s e -> unP (runECP_P e) s
    PFailed ps -> PFailed ps
#else
parseExpression = parse Parser.parseExpression
#endif

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

#if defined(GHCLIB_API_901)
parseHeader :: String -> DynFlags -> ParseResult (Located HsModule)
#else
parseHeader :: String -> DynFlags -> ParseResult (Located (HsModule GhcPs))
#endif
parseHeader = parse Parser.parseHeader

#if defined (GHCLIB_API_901)
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
