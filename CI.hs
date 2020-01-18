-- Copyright (c) 2020, Shayne Fletcher. All rights reserved.
-- SPDX-License-Identifier: BSD-3-Clause.

-- CI script, compatible with all of Travis, Appveyor and Azure.
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
import Control.Monad.Extra
import System.Directory
import System.FilePath
import System.IO.Extra
import System.Info.Extra
import System.Process.Extra
import System.Time.Extra
import Data.List.Extra
import Data.Time.Clock
import Data.Time.Calendar
import Data.Semigroup ((<>))
import Data.Maybe
import qualified Options.Applicative as Opts
import qualified System.Environment as Env
import qualified System.Exit as Exit

main :: IO ()
main = do
    let opts =
          Opts.info (parseOptions Opts.<**> Opts.helper)
          ( Opts.fullDesc
            <> Opts.progDesc "Build  and test ghc-lib-parser-ex."
            <> Opts.header "CI - CI script for ghc-lib-parser-ex"
          )
    Options { stackOptions } <- Opts.execParser opts
    _ <- buildDist stackOptions
    return ()

newtype Options = Options
    { stackOptions :: StackOptions
    } deriving (Show)

data StackOptions = StackOptions
    { stackYaml :: Maybe String -- Optional config file
    , resolver :: Maybe String  -- If 'Just _', override stack.yaml.
    , verbosity :: Maybe String -- If provided, pass '--verbosity=xxx' to 'stack build'. Valid values are "silent",  "error", "warn", "info" or "debug".
    , cabalVerbose :: Bool -- If enabled, pass '--cabal-verbose' to 'stack build'.
    , ghcOptions :: Maybe String -- If 'Just _', pass '--ghc-options="xxx"' to 'stack build' (for ghc verbose, try 'v3').
    , versionTag :: Maybe String -- If 'Just _' use this as the version (e.g. "8.8.1.20191204")
    } deriving (Show)

-- Command line argument generators.

stackYamlOpt :: Maybe String -> String
stackYamlOpt = \case
  Just file -> "--stack-yaml " ++ file
  Nothing -> ""

stackResolverOpt :: Maybe String -> String
stackResolverOpt = \case
  Just resolver -> "--resolver " ++ resolver
  Nothing -> ""

stackVerbosityOpt :: Maybe String -> String
stackVerbosityOpt = \case
  Just verbosity -> "--verbosity=" ++ verbosity
  Nothing -> ""

cabalVerboseOpt :: Bool -> String
cabalVerboseOpt True = "--cabal-verbose"; cabalVerboseOpt False = ""

ghcOptionsOpt :: Maybe String -> String
ghcOptionsOpt = \case
  Just options -> "--ghc-options=\"" ++ options ++ "\""
  Nothing -> ""

-- Calculate a version string based on a date and a ghc flavor.
genVersionStr :: Day -> String
genVersionStr day = "0." ++ replace "-" "" (showGregorian day)

parseOptions :: Opts.Parser Options
parseOptions = Options
    <$> parseStackOptions

parseStackOptions :: Opts.Parser StackOptions
parseStackOptions = StackOptions
    <$> Opts.optional ( Opts.strOption
        ( Opts.long "stack-yaml"
          <> Opts.help "If specified, the stack-yaml file to use"
        ))
    <*> Opts.optional ( Opts.strOption
        ( Opts.long "resolver"
       <> Opts.help "If specified, pass '--resolver=xxx' to stack"
        ))
    <*> Opts.optional ( Opts.strOption
        ( Opts.long "verbosity"
       <> Opts.help "If specified, pass '--verbosity=xxx' to stack"
        ))
    <*> Opts.switch
        ( Opts.long "cabal-verbose"
       <> Opts.help "If specified, pass '--cabal-verbose' to stack"
        )
    <*> Opts.optional ( Opts.strOption
        ( Opts.long "ghc-options"
       <> Opts.help "If specified, pass '--ghc-options=\"xxx\"' to stack"
        ))
    <*> Opts.optional ( Opts.strOption
        ( Opts.long "version-tag"
       <> Opts.help "If specified, set this as the version in ghc-lib-parser-ex.cabal"
        ))

buildDist :: StackOptions -> IO String
buildDist StackOptions {stackYaml, resolver, verbosity, cabalVerbose, ghcOptions, versionTag} =
  do
    -- Clear up any detritus left over from previous runs.
    toDelete <- (["ghc-lib-parser-ex"] ++) .
      filter (isExtensionOf ".tar.gz") <$> getDirectoryContents "."
    forM_ toDelete removePath
    cmd "git checkout stack.yaml"

    -- Get packages missing on Windows needed by hadrian.
    when isWindows $
        stack "exec -- pacman -S autoconf automake-wrapper make patch python tar --noconfirm"

    -- Calculate verison and package names.
    version <- tag
    let pkg_ghclib_parser_ex = "ghc-lib-parser-ex-" ++ version

    -- Make an sdist of the package and extract it.
    patchVersion version "ghc-lib-parser-ex.cabal"
    patchConstraint version "ghc-lib-parser-ex.cabal"
    mkTarball pkg_ghclib_parser_ex
    renameDirectory pkg_ghclib_parser_ex "ghc-lib-parser-ex"
    -- cmd "git checkout ghc-lib-parser-ex.cabal"

    -- Update stack.yaml to reference the newly extracted package.
    let config = fromMaybe "stack.yaml" stackYaml
    writeFile config .
      replace "- ." "- ghc-lib-parser-ex"
        =<< readFile' config

    -- Build and test the package.
    stack $ "--no-terminal --interleaved-output " ++ "build " ++ ghcOptionsOpt ghcOptions  ++ " ghc-lib-parser-ex"
    stack $ "--no-terminal --interleaved-output " ++ "test " ++ ghcOptionsOpt ghcOptions  ++ " ghc-lib-parser-ex"

    -- Something like, "0.20200112".
    tag  -- The return value of type 'IO string'.

    where
      stack :: String -> IO ()
      stack action = cmd $ "stack " ++
        concatMap (<> " ")
                 [ stackYamlOpt stackYaml
                 , stackResolverOpt resolver
                 , stackVerbosityOpt verbosity
                 , cabalVerboseOpt cabalVerbose
                 ] ++
        action

      cmd :: String -> IO ()
      cmd x = do
        putStrLn $ "\n\n# Running: " ++ x
        hFlush stdout
        (t, _) <- duration $ system_ x
        putStrLn $ "# Completed in " ++ showDuration t ++ ": " ++ x ++ "\n"
        hFlush stdout

      mkTarball :: String -> IO ()
      mkTarball target = do
        stack "sdist . --tar-dir=."
        cmd $ "tar -xvf " ++ target ++ ".tar.gz"

      tag :: IO String
      tag = case versionTag of
          Just t -> return t
          _ -> do
            UTCTime day _ <- getCurrentTime
            return $ genVersionStr day

      patchVersion :: String -> FilePath -> IO ()
      patchVersion version file =
        writeFile file .
          replace "version:        0.1.0" ("version:        " ++ version)
          =<< readFile' file

      patchConstraint :: String -> FilePath -> IO ()
      patchConstraint version file =
        when (isJust versionTag) $
          writeFile file .
            replace
            "      build-depends:\n        ghc-lib-parser"
            ("      build-depends:\n        ghc-lib-parser == " ++ version)
            =<< readFile' file

      removePath :: FilePath -> IO ()
      removePath p =
        whenM (doesPathExist p) $ do
          putStrLn $ "# Removing " ++ p
          removePathForcibly p
