-- Copyright (c) 2020, 2024 Shayne Fletcher. All rights reserved.
-- SPDX-License-Identifier: BSD-3-Clause.

{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE CPP #-}

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
    Options { versionTag } <- Opts.execParser opts
    buildDist versionTag

data Options = Options {
    versionTag :: Maybe String -- If 'Just _' use this as the version (e.g. "8.8.1.20191204")
  } deriving (Show)

buildDist :: Maybe String -> IO ()
buildDist versionTag = do
  version <- tag
  patchCabal version
  contents <- readFile' "ghc-lib-parser-ex.cabal"
  putStrLn contents
  hFlush stdout

 -- --test-show-details direct --test-options \"--color always
  system_ "cabal sdist -o ."
  system_ "cabal build lib:ghc-lib-parser-ex --ghc-options=-j"
  system_ "cabal test test:ghc-lib-parser-ex-test --ghc-options=-j --test-show-details=direct --test-options=\"--color always\""
#if __GLASGOW_HASKELL__ == 808 && \
    (__GLASGOW_HASKELL_PATCHLEVEL1__ == 1 || __GLASGOW_HASKELL_PATCHLEVEL1__ == 2) && \
    defined (mingw32_HOST_OS)
      -- https://gitlab.haskell.org/ghc/ghc/issues/17599
#else
  system_ "cabal exec -- ghc -ignore-dot-ghci -package=ghc-lib-parser-ex -e \"print 1\""
#endif

  where
    tag :: IO String
    tag = maybe (do UTCTime day _ <- getCurrentTime; pure $ genVersionStr day) pure versionTag

    genVersionStr :: Day -> String
    genVersionStr day = "0." ++ replace "-" "" (showGregorian day)

parseOptions :: Opts.Parser Options
parseOptions = Options
  <$> Opts.optional ( Opts.strOption (
        Opts.long "version-tag" <> Opts.help "Set version"
    ))

patchCabal :: String -> IO ()
patchCabal version = do
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
