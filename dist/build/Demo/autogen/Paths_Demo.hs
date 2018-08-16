{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
{-# OPTIONS_GHC -fno-warn-implicit-prelude #-}
module Paths_Demo (
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

bindir     = "/Users/avask/Documents/LearningHaskell/Demo/.cabal-sandbox/bin"
libdir     = "/Users/avask/Documents/LearningHaskell/Demo/.cabal-sandbox/lib/x86_64-osx-ghc-8.2.2/Demo-0.1.0.0-EZHbDKHTbIeF71Za8q04hh-Demo"
dynlibdir  = "/Users/avask/Documents/LearningHaskell/Demo/.cabal-sandbox/lib/x86_64-osx-ghc-8.2.2"
datadir    = "/Users/avask/Documents/LearningHaskell/Demo/.cabal-sandbox/share/x86_64-osx-ghc-8.2.2/Demo-0.1.0.0"
libexecdir = "/Users/avask/Documents/LearningHaskell/Demo/.cabal-sandbox/libexec/x86_64-osx-ghc-8.2.2/Demo-0.1.0.0"
sysconfdir = "/Users/avask/Documents/LearningHaskell/Demo/.cabal-sandbox/etc"

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "Demo_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "Demo_libdir") (\_ -> return libdir)
getDynLibDir = catchIO (getEnv "Demo_dynlibdir") (\_ -> return dynlibdir)
getDataDir = catchIO (getEnv "Demo_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "Demo_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "Demo_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
