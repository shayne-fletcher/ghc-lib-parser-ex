-- Copyright (c) 2020, Shayne Fletcher. All rights reserved.
-- SPDX-License-Identifier: BSD-3-Clause.

{-# LANGUAGE TupleSections #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE CPP #-}

-- CI script, compatible with all of Travis, Appveyor and Azure.
module CI (main) where

import Control.Monad.Extra
import System.Directory
import System.FilePath
import System.IO.Extra
import System.Info.Extra
import System.Process.Extra
import System.Time.Extra
import System.Console.ANSI
import Data.List.Extra
import Data.Time.Clock
import Data.Time.Calendar
import Data.Semigroup ((<>))
import Data.Foldable
import Data.Maybe
import qualified Options.Applicative as Opts
import qualified System.Environment as Env
import qualified System.Exit as Exit
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
    Options { stackOptions } <- Opts.execParser opts
    buildDist stackOptions

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

buildDist :: StackOptions -> IO ()
buildDist opts = do
    when isWindows $
      stack' opts "exec -- pacman -S autoconf automake-wrapper make patch python tar --noconfirm"
    isolatedBuild opts

isolatedBuild opts@StackOptions {..} = do
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
      writeFile "ghc-lib-parser-ex.cabal" .
          replace "version:        0.1.0" ("version:        " ++ version)
          =<< readFile' "ghc-lib-parser-ex.cabal"
      stack "sdist . --tar-dir=."
      copyFile (pkg_ghclib_parser_ex <> ".tar.gz") (rootDir </> (pkg_ghclib_parser_ex <> ".tar.gz"))
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

stack' :: StackOptions -> String -> IO ()
stack' StackOptions {stackYaml, resolver, verbosity, cabalVerbose} action =
  cmd $ "stack " ++
    concatMap (<> " ")
    [ stackYamlOpt stackYaml
    , stackResolverOpt resolver
    , stackVerbosityOpt verbosity
    , cabalVerboseOpt cabalVerbose
    ] ++
  action

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
