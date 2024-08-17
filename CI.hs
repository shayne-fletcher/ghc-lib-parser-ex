-- Copyright (c) 2020, 2024 Shayne Fletcher. All rights reserved.
-- SPDX-License-Identifier: BSD-3-Clause.
{-# LANGUAGE CPP #-}
{-# LANGUAGE NamedFieldPuns #-}

import Control.Monad.Extra
#if __GLASGOW_HASKELL__ < 906
import Control.Applicative (liftA2)
#endif
import Data.Bifunctor
import Data.List.Extra
import Data.Time.Calendar
import Data.Time.Clock
import qualified Options.Applicative as Opts
import System.Exit
import System.IO.Extra
import System.Process.Extra

main :: IO ()
main = do
  let opts =
        Opts.info
          (parseOptions Opts.<**> Opts.helper)
          ( Opts.fullDesc
              <> Opts.progDesc "Build and test ghc-lib-parser-ex."
              <> Opts.header "CI - CI script for ghc-lib-parser-ex"
          )
  options <- Opts.execParser opts
  buildTestDist options

buildTestDist :: Options -> IO ()
buildTestDist Options {versionTag = userGivenTag, noBuilds} = do
  assignVersionPatchCabal userGivenTag

  system_ "cabal sdist -o ."
  when noBuilds exitSuccess
  system_ "cabal build lib:ghc-lib-parser-ex --ghc-options=-j"
  system_ "cabal test test:ghc-lib-parser-ex-test --ghc-options=-j --test-show-details=direct --test-options=\"--color always\""
#if __GLASGOW_HASKELL__ == 808 && \
    (__GLASGOW_HASKELL_PATCHLEVEL1__ == 1 || __GLASGOW_HASKELL_PATCHLEVEL1__ == 2) && \
    defined (mingw32_HOST_OS)
      -- https://gitlab.haskell.org/ghc/ghc/issues/17599
#else
  system_ "cabal exec -- ghc -ignore-dot-ghci -package=ghc-lib-parser-ex -e \"print 1\""
#endif

assignVersionPatchCabal :: Maybe String -> IO ()
assignVersionPatchCabal userTag = do
  version <- mkVersion userTag
  patchCabal version
  contents <- readFile' "ghc-lib-parser-ex.cabal"
  putStrLn contents
  hFlush stdout
  where
    mkVersion :: Maybe String -> IO String
    mkVersion = maybe (do UTCTime day _ <- getCurrentTime; pure $ genVersionStr day) pure

    genVersionStr :: Day -> String
    genVersionStr day = "0." ++ replace "-" "" (showGregorian day)

patchCabal :: String -> IO ()
patchCabal version = do
  putStrLn "Patching cabal:"
  putStrLn $ "- version " ++ version
  writeFile "ghc-lib-parser-ex.cabal"
    . replace "version: 0.1.0" ("version: " ++ version)
    =<< readFile' "ghc-lib-parser-ex.cabal"
  let series
        | Just (major, Just (minor, _)) <- second (stripInfix ".") <$> stripInfix "." version = liftA2 (,) (maybeRead major) (maybeRead minor)
        | otherwise = Nothing
  case series of
    Just (major, minor) -> do
      let (lower, upper, family) = bounds (major, minor)
      putStrLn $ "- ghc >= " ++ lower ++ " && ghc < " ++ upper
      putStrLn $ "- ghc-lib-parser " ++ family
      writeFile "ghc-lib-parser-ex.cabal"
        . replace "9.0.0" lower
        . replace "9.1.0" upper
        . replace "9.0.*" family
        =<< readFile' "ghc-lib-parser-ex.cabal"
    Nothing -> do
      let ghcLibParserVersion = version
      putStrLn $ "- ghc-lib-parser " ++ ghcLibParserVersion
      writeFile "ghc-lib-parser-ex.cabal"
        . replace
          buildDependsSection
          ( unlines
              [ "  build-depends:",
                "      ghc-lib-parser == " ++ ghcLibParserVersion,
                "  -- pseduo use of flags to suppress cabal check warnings",
                "  if flag(auto)",
                "  if flag(no-ghc-lib)"
              ]
          )
        =<< readFile' "ghc-lib-parser-ex.cabal"
  where
    maybeRead :: String -> Maybe Int
    maybeRead s
      | [(x, "")] <- reads s = Just x
      | otherwise = Nothing

    buildDependsSection :: String
    buildDependsSection =
      unlines
        [ "  if flag(auto) && impl(ghc >= 9.0.0) && impl(ghc < 9.1.0)",
          "    build-depends:",
          "      ghc == 9.0.*,",
          "      ghc-boot-th,",
          "      ghc-boot",
          "  else",
          "    if flag(auto)",
          "      build-depends:",
          "        ghc-lib-parser == 9.0.*",
          "    else",
          "      if flag(no-ghc-lib)",
          "        build-depends:",
          "          ghc == 9.0.*,",
          "          ghc-boot-th,",
          "          ghc-boot",
          "      else",
          "        build-depends:",
          "          ghc-lib-parser == 9.0.*"
        ]

    bounds :: (Int, Int) -> (String, String, String)
    bounds (major, minor) =
      let lower = if lower' == "9.2.0" then "9.2.2" else lower'
          upper = upper'
          family = family'
       in (lower, upper, family)
      where
        lower' = show major ++ "." ++ show minor ++ ".0"
        upper' = show major ++ "." ++ show (minor + 1) ++ ".0"
        family' = show major ++ "." ++ show minor ++ ".*"

data Options = Options
  { versionTag :: Maybe String, -- If 'Just _' use this as the version (e.g. "8.8.1.20191204")
    noBuilds :: Bool -- Don't build or run tests
  }
  deriving (Show)

parseOptions :: Opts.Parser Options
parseOptions =
  Options
    <$> Opts.optional
      ( Opts.strOption
          (Opts.long "version-tag" <> Opts.help "Set version")
      )
    <*> Opts.switch
      (Opts.long "no-builds" <> Opts.help "If enabled, stop after producing sdists")
