-- Copyright (c) 2020-2023, Shayne Fletcher. All rights reserved.
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

#if defined (GHC_9_10) || defined (GHC_9_8) || defined (GHC_9_6) || defined (GHC_9_4) || defined(GHC_9_2) || defined (GHC_9_0) || defined (GHC_8_10)
import GHC.Hs
#else
import HsSyn
#endif
#if defined (GHC_9_10) || defined (GHC_9_8) || defined (GHC_9_6) || defined (GHC_9_4) || defined(GHC_9_2)
#  if defined (GHC_9_2)
import GHC.Driver.Config
#  endif
#  if defined (GHC_9_10) || defined (GHC_9_8) || defined (GHC_9_6) || defined (GHC_9_4)
import GHC.Driver.Config.Parser
#  endif
#endif
#if defined (GHC_9_10) || defined (GHC_9_8) || defined (GHC_9_6) || defined (GHC_9_4) || defined(GHC_9_2) || defined (GHC_9_0)
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
#if defined (GHC_8_10)
import RdrHsSyn
#endif

parse :: P a -> String -> DynFlags -> ParseResult a
parse p str flags =
  Lexer.unP p parseState
  where
    location = mkRealSrcLoc (mkFastString "<string>") 1 1
    buffer = stringToStringBuffer str
    parseState =
#if defined (GHC_9_10) || defined (GHC_9_8) || defined (GHC_9_6) || defined (GHC_9_4) || defined(GHC_9_2)
      initParserState (initParserOpts flags) buffer location
#else
      mkPState flags buffer location
#endif
#if defined (GHC_9_4) || defined(GHC_9_2) || defined (GHC_9_0)
parseModule :: String -> DynFlags -> ParseResult (Located HsModule)
#else
parseModule :: String -> DynFlags -> ParseResult (Located (HsModule GhcPs))
#endif
parseModule = parse Parser.parseModule

#if defined (GHC_9_4) || defined(GHC_9_2) || defined (GHC_9_0)
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
#if defined (GHC_9_10) || defined (GHC_9_8) || defined (GHC_9_6) || defined (GHC_9_4) || defined(GHC_9_2)
parseExpression s flags =
  -- The need for annotations here came about first manifested with
  -- ghc-9.0.1
  case parse Parser.parseExpression s flags of
    POk state e ->
      let e' = e :: ECP
          parser_validator = unECP e' :: PV (LHsExpr GhcPs)
          parser = runPV parser_validator :: P (LHsExpr GhcPs)
      in unP parser state :: ParseResult (LHsExpr GhcPs)
    PFailed ps -> PFailed ps
#elif defined (GHC_8_10) || defined (GHC_9_0)
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

#if defined (GHC_9_10) || defined (GHC_9_8) || defined (GHC_9_6) || defined (GHC_9_4) || defined (GHC_9_2)
parseIdentifier :: String -> DynFlags -> ParseResult (LocatedN RdrName)
#else
parseIdentifier :: String -> DynFlags -> ParseResult (Located RdrName)
#endif
parseIdentifier = parse Parser.parseIdentifier

parseType :: String -> DynFlags -> ParseResult (LHsType GhcPs)
parseType = parse Parser.parseType

#if defined (GHC_9_4) || defined(GHC_9_2) || defined (GHC_9_0)
parseHeader :: String -> DynFlags -> ParseResult (Located HsModule)
#else
parseHeader :: String -> DynFlags -> ParseResult (Located (HsModule GhcPs))
#endif
parseHeader = parse Parser.parseHeader

#if defined (GHC_9_4) || defined(GHC_9_2) || defined (GHC_9_0)
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
    parseState =
#if defined (GHC_9_10) || defined (GHC_9_8) || defined (GHC_9_6) || defined (GHC_9_4) || defined(GHC_9_2)
      initParserState (initParserOpts flags) buffer location
#else
      mkPState flags buffer location
#endif
