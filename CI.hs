-- Copyright (c) 2020, 2021 Shayne Fletcher. All rights reserved.
-- SPDX-License-Identifier: BSD-3-Clause.

{-# OPTIONS_GHC -fno-warn-name-shadowing #-}
{-# OPTIONS_GHC -fwarn-unused-imports #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE CPP #-}

-- CI script, compatible with all of Travis, Appveyor and Azure.
import System.IO.Extra
import System.Process.Extra
import Data.List.Extra
import Data.Time.Clock
import Data.Time.Calendar
import Data.Bifunctor
import qualified Options.Applicative as Opts

main :: IO ()
main = do
    let opts =
          Opts.info (parseOptions Opts.<**> Opts.helper)
          ( Opts.fullDesc
            <> Opts.progDesc "Build and test ghc-lib-parser-ex."
            <> Opts.header "CI - CI script for ghc-lib-parser-ex"
          )
    Options { stackOptions, noBuilds } <- Opts.execParser opts
    buildDist stackOptions noBuilds

data Options = Options
    { stackOptions :: StackOptions
    , noBuilds :: Bool
    } deriving (Show)

data StackOptions = StackOptions
    { stackYaml :: Maybe String -- Optional config file
    , resolver :: Maybe String  -- If 'Just _', override stack.yaml.
    , verbosity :: Maybe String -- If provided, pass '--verbosity=xxx' to 'stack build'. Valid values are "silent",  "error", "warn", "info" or "debug".
    , cabalVerbose :: Bool -- If enabled, pass '--cabal-verbose' to 'stack build'.
    , ghcOptions :: Maybe String -- If 'Just _', pass '--ghc-options="xxx"' to 'stack build' (for ghc verbose, try 'v3').
    , versionTag :: Maybe String -- If 'Just _' use this as the version (e.g. "8.8.1.20191204")
    } deriving (Show)

parseOptions :: Opts.Parser Options
parseOptions = Options
  <$> parseStackOptions
  <*> Opts.switch
  (
    Opts.long "no-builds" <> Opts.help "If enabled, stop after producing sdists"
  )

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

buildDist :: StackOptions -> Bool -> IO ()
buildDist opts noBuilds = isolatedBuild opts noBuilds

patchCabal :: String -> StackOptions -> IO ()
patchCabal version _opts = do
  putStrLn "Patching cabal:"
  putStrLn $ "- version " ++ version
  writeFile "ghc-lib-parser-ex.cabal" .
      replace "version: 0.1.0" ("version: " ++ version)
      =<< readFile' "ghc-lib-parser-ex.cabal"
  let series =
        case second (stripInfix ".") <$> stripInfix "." version of
          Just (major, Just (minor, _)) ->
            liftA2 (,) (maybeRead major) (maybeRead minor)
          _ -> Nothing
  case series of
    -- If the version is of form major.minor.* parse out the major and
    -- minor numbers and patch ghc/ghc-lib-parser bounds with them.
    Just (major, minor) -> do
      let family = show major ++ "." ++ show minor ++ ".*"
      let lower  = show major ++ "." ++ show minor ++ ".0"
      let upper  = show major ++ "." ++ show (minor + 1) ++ ".0"
      -- Tiny hack to skip past 9.2.1 (see https://github.com/ndmitchell/hlint/issues/1314)
      lower <- pure $ if lower == "9.2.0" then "9.2.2" else lower
      putStrLn $ "- ghc >= " ++ lower ++ " && ghc < " ++ upper
      putStrLn $ "- ghc-lib-parser " ++ family
      writeFile "ghc-lib-parser-ex.cabal" .
        replace "9.0.0" lower .
        replace "9.1.0" upper .
        replace "9.0.*" family
        =<< readFile' "ghc-lib-parser-ex.cabal"
    Nothing -> do
      let ghcLibParserVersion = version
      putStrLn $ "- ghc-lib-parser " ++ ghcLibParserVersion
      writeFile "ghc-lib-parser-ex.cabal" .
        replace section (
          unlines [
              "  build-depends:",
              "      ghc-lib-parser == " ++ ghcLibParserVersion,
              "  -- pseduo use of flags to suppress cabal check warnings",
              "  if flag(auto)",
              "  if flag(no-ghc-lib)"
              ])
        =<< readFile' "ghc-lib-parser-ex.cabal"

  where
    maybeRead :: String -> Maybe Int
    maybeRead s
      | [(x, "")] <- reads s = Just x
      | otherwise = Nothing

    section :: String
    section = unlines [
        "  if flag(auto) && impl(ghc >= 9.0.0) && impl(ghc < 9.1.0)"
      , "    build-depends:"
      , "      ghc == 9.0.*,"
      , "      ghc-boot-th,"
      , "      ghc-boot"
      , "  else"
      , "    if flag(auto)"
      , "      build-depends:"
      , "        ghc-lib-parser == 9.0.*"
      , "    else"
      , "      if flag(no-ghc-lib)"
      , "        build-depends:"
      , "          ghc == 9.0.*,"
      , "          ghc-boot-th,"
      , "          ghc-boot"
      , "      else"
      , "        build-depends:"
      , "          ghc-lib-parser == 9.0.*"
      ]

isolatedBuild :: StackOptions -> Bool -> IO ()
isolatedBuild opts@StackOptions {..} _noBuilds = do
  version <- tag
  patchCabal version opts
  contents <- readFile' "ghc-lib-parser-ex.cabal"
  putStrLn contents
  hFlush stdout

  system_ "cabal sdist -o ."
  -- when noBuilds exitSuccess
  system_ "cabal build lib:ghc-lib-parser-ex --ghc-options=-j"
  system_ "cabal test test:ghc-lib-parser-ex-test --ghc-options=-j"
-- #if __GLASGOW_HASKELL__ == 808 && \
--     (__GLASGOW_HASKELL_PATCHLEVEL1__ == 1 || __GLASGOW_HASKELL_PATCHLEVEL1__ == 2) && \
--     defined (mingw32_HOST_OS)
--       -- https://gitlab.haskell.org/ghc/ghc/issues/17599
-- #else
--   -- system_ "unset GHC_PACKAGE_PATH && cabal exec -- ghc -ignore-dot-ghci -package=ghc-lib-parser-parser-ex -e \"print 1\""
-- #endif
    where
      tag :: IO String
      tag = maybe (do UTCTime day _ <- getCurrentTime; pure $ genVersionStr day) pure versionTag

      genVersionStr :: Day -> String
      genVersionStr day = "0." ++ replace "-" "" (showGregorian day)

-- Mitigate against macOS/ghc-9.2.2 failures for lack of this
-- c-include path. See
-- https://gitlab.haskell.org/ghc/ghc/-/issues/20592#note_391266.
-- There are reports that this exhibits with 9.0.2 and 9.2.1 as
-- well but I haven't observed that.
-- prelude :: (String, String) -> String
-- #if __GLASGOW_HASKELL__ == 902 && __GLASGOW_HASKELL_PATCHLEVEL1__ == 2
-- prelude ("darwin", _) = "C_INCLUDE_PATH=`xcrun --show-sdk-path`/usr/include/ffi "
-- #endif
-- prelude _ = ""
