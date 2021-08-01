{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
module Paths_command_line_life (
    version,
    getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir,
    getDataFileName, getSysconfDir
  ) where

import qualified Control.Exception as Exception
import Data.Version (Version(..))
import System.Environment (getEnv)
import Prelude

#if defined(VERSION_base)

#if MIN_VERSION_base(4,0,0)
catchIO :: IO a -> (Exception.IOException -> IO a) -> IO a
#else
catchIO :: IO a -> (Exception.Exception -> IO a) -> IO a
#endif

#else
catchIO :: IO a -> (Exception.IOException -> IO a) -> IO a
#endif
catchIO = Exception.catch

version :: Version
version = Version [0,1,0,0] []
bindir, libdir, dynlibdir, datadir, libexecdir, sysconfdir :: FilePath

bindir     = "/Users/dtuveson/.cabal/bin"
libdir     = "/Users/dtuveson/.cabal/lib/x86_64-osx-ghc-8.10.4/command-line-life-0.1.0.0-inplace-command-line-life"
dynlibdir  = "/Users/dtuveson/.cabal/lib/x86_64-osx-ghc-8.10.4"
datadir    = "/Users/dtuveson/.cabal/share/x86_64-osx-ghc-8.10.4/command-line-life-0.1.0.0"
libexecdir = "/Users/dtuveson/.cabal/libexec/x86_64-osx-ghc-8.10.4/command-line-life-0.1.0.0"
sysconfdir = "/Users/dtuveson/.cabal/etc"

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "command_line_life_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "command_line_life_libdir") (\_ -> return libdir)
getDynLibDir = catchIO (getEnv "command_line_life_dynlibdir") (\_ -> return dynlibdir)
getDataDir = catchIO (getEnv "command_line_life_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "command_line_life_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "command_line_life_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
