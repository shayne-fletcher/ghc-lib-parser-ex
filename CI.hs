-- Copyright (c) 2020, 2021 Shayne Fletcher. All rights reserved.
-- SPDX-License-Identifier: BSD-3-Clause.

{-# LANGUAGE TupleSections #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE CPP #-}

-- CI script, compatible with all of Travis, Appveyor and Azure.
module CI (main) where

import Control.Monad.Extra
import Control.Applicative
import System.Directory
import System.FilePath
import System.IO.Extra
import System.Info.Extra
import System.Process.Extra
import System.Time.Extra
import System.Console.ANSI
import System.Exit
import Data.List.Extra
import Data.Time.Clock
import Data.Time.Calendar
import Data.Foldable
import Data.Maybe
import Data.Bifunctor
import qualified Options.Applicative as Opts
import GHC.Stack
import System.IO.Unsafe

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
buildDist opts noBuilds = do
    when isWindows $
      stack' opts "exec -- pacman -S autoconf automake-wrapper make patch python tar --noconfirm"
    isolatedBuild opts noBuilds

patchCabal :: String -> StackOptions -> IO ()
patchCabal version opts = do
  setSGR [SetColor Foreground Dull Red]
  putStrLn "Patching cabal:"
  putStrLn $ "- version " ++ version
  writeFile "ghc-lib-parser-ex.cabal" .
      replace "version:        0.1.0" ("version:        " ++ version)
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
      -- If version is of the form 0.x, depend strictly on
      -- ghc-lib-parser and don't constrain its bounds.
      out <- dropSuffix "\n" <$> systemOutput_ (
        stackWithOpts opts ++ " ls dependencies --depth 1"
        )
      let ls = filter ("ghc-lib-parser " `isPrefixOf`) $ map trim (lines out)
      let ghcLibParserVersion =
            case map (stripPrefix "ghc-lib-parser ") ls of
              [Just version] -> version
              _ -> fail "Couldn't determine ghc-lib-parser version"
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

  setSGR [Reset]
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
isolatedBuild opts@StackOptions {..} noBuilds = do
  version <- tag
  stackYaml <- pure $ fromMaybe "stack.yaml" stackYaml
  let pkg_ghclib_parser_ex = "ghc-lib-parser-ex-" ++ version
  rootDir <- getCurrentDirectory
  withTempDir $ \tmpDir -> do
    copyFilesWith copyFile tmpDir $ map (rootDir,)
      [ "LICENSE", "README.md", "ChangeLog.md", "ghc-lib-parser-ex.cabal", "Setup.hs", stackYaml ]
    copyDirectoryRecursive (rootDir </> "cbits") (tmpDir </> "cbits")
    copyDirectoryRecursive (rootDir </> "src") (tmpDir </> "src")
    copyDirectoryRecursive (rootDir </> "test") (tmpDir </> "test")
    withCurrentDirectory tmpDir $ do
      patchCabal version opts
      contents <- readFile' "ghc-lib-parser-ex.cabal"
      putStrLn contents
      stack "sdist . --tar-dir=."
      copyFile (pkg_ghclib_parser_ex <> ".tar.gz") (rootDir </> (pkg_ghclib_parser_ex <> ".tar.gz"))

      when noBuilds exitSuccess

      cmd $ "tar -xvf " <> (pkg_ghclib_parser_ex  <> ".tar.gz")
      renameDirectory pkg_ghclib_parser_ex "ghc-lib-parser-ex"
      writeFile stackYaml . replace "- ." "- ghc-lib-parser-ex" =<< readFile' stackYaml
      -- Build and test the package.
      stack $ "--no-terminal --interleaved-output " ++ "build " ++ ghcOptionsOpt ghcOptions  ++ " ghc-lib-parser-ex"
      stack $ "--no-terminal --interleaved-output " ++ "test " ++ ghcOptionsOpt ghcOptions  ++ " ghc-lib-parser-ex"
#if __GLASGOW_HASKELL__ == 808 && \
    (__GLASGOW_HASKELL_PATCHLEVEL1__ == 1 || __GLASGOW_HASKELL_PATCHLEVEL1__ == 2) && \
    defined (mingw32_HOST_OS)
      -- Skip these tests on ghc-8.8.1 and 8.8.2 (exclusively). See
      -- https://gitlab.haskell.org/ghc/ghc/issues/17599.
#else
      -- Test everything loads in GHCi, see
      -- https://github.com/digital-asset/ghc-lib/issues/27
      stack "--no-terminal exec -- ghc -ignore-dot-ghci -package=ghc-lib-parser-ex -e \"print 1\""
#endif
    where
      tag :: IO String
      tag = maybe (do UTCTime day _ <- getCurrentTime; pure $ genVersionStr day) pure versionTag

      stack = stack' opts

-- Mitigate against macOS/ghc-9.2.2 failures for lack of this
-- c-include path. See
-- https://gitlab.haskell.org/ghc/ghc/-/issues/20592#note_391266.
-- There are reports that this exhibits with 9.0.2 and 9.2.1 as
-- well but I haven't observed that.
prelude :: (String, String) -> String
#if __GLASGOW_HASKELL__ == 902 && __GLASGOW_HASKELL_PATCHLEVEL1__ == 2
prelude ("darwin", _) = "C_INCLUDE_PATH=`xcrun --show-sdk-path`/usr/include/ffi "
#endif
prelude _ = ""

stack' :: StackOptions -> String -> IO ()
stack' opts action = cmd $ prelude (os, arch) ++ stackWithOpts opts ++ action

stackWithOpts :: StackOptions -> String
stackWithOpts StackOptions {stackYaml, resolver, verbosity, cabalVerbose} =
 "stack " ++ concatMap (<> " ")
  [ stackYamlOpt stackYaml
  , stackResolverOpt resolver
  , stackVerbosityOpt verbosity
  , cabalVerboseOpt cabalVerbose
  ]

cmd :: String -> IO ()
cmd x = do
  setSGR [SetColor Foreground Dull Cyan]
  putStrLn x
  hFlush stdout
  (t, _) <- duration $ system_ x
  setSGR [SetColor Foreground Dull Cyan]
  putStrLn $ "Completed " ++ showDuration t ++ ": " ++ x ++ "\n"
  hFlush stdout
  setSGR [Reset]

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

-- The following functions are copied from Cabal
-- 'Distribution.Simple.Utils' to avoid a dependency of this script on
-- that package (it's problematic with the 8.8.1/2 Windows relocation
-- 0 bug).

copyDirectoryRecursive :: FilePath -> FilePath -> IO ()
copyDirectoryRecursive srcDir destDir = withFrozenCallStack $ do
  srcFiles <- getDirectoryContentsRecursive srcDir
  copyFilesWith copyFile destDir [ (srcDir, f) | f <- srcFiles ]

getDirectoryContentsRecursive :: FilePath -> IO [FilePath]
getDirectoryContentsRecursive topdir = recurseDirectories [""]
  where
    recurseDirectories :: [FilePath] -> IO [FilePath]
    recurseDirectories [] = return []
    recurseDirectories (dir:dirs) = unsafeInterleaveIO $ do
      (files, dirs') <- collect [] [] =<< getDirectoryContents (topdir </> dir)
      files' <- recurseDirectories (dirs' ++ dirs)
      return (files ++ files')
      where
        collect files dirs' [] = return (reverse files, reverse dirs')
        collect files dirs' (entry:entries) | ignore entry = collect files dirs' entries
        collect files dirs' (entry:entries) = do
          let dirEntry = dir </> entry
          isDirectory <- doesDirectoryExist (topdir </> dirEntry)
          if isDirectory
            then collect files (dirEntry:dirs') entries
            else collect (dirEntry:files) dirs' entries
        ignore ['.'] = True
        ignore ['.', '.'] = True
        ignore _ = False

copyFilesWith :: (FilePath -> FilePath -> IO ()) -> FilePath -> [(FilePath, FilePath)] -> IO ()
copyFilesWith doCopy targetDir srcFiles = withFrozenCallStack $ do
  let dirs = map (targetDir </>) . nub . map (takeDirectory . snd) $ srcFiles
  traverse_ (createDirectoryIfMissing True) dirs
  sequence_ [ let src = srcBase </> srcFile
                  dest = targetDir </> srcFile
               in doCopy src dest
            | (srcBase, srcFile) <- srcFiles ]
