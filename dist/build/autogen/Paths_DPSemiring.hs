{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
module Paths_DPSemiring (
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

bindir     = "/nfs/stak/users/kumarpra/.cabal/bin"
libdir     = "/nfs/stak/users/kumarpra/.cabal/lib/x86_64-linux-ghc-8.4.3/DPSemiring-0.1.0.0-3pb0YLKU7mJ5DdJ7XBaRqF"
dynlibdir  = "/nfs/stak/users/kumarpra/.cabal/lib/x86_64-linux-ghc-8.4.3"
datadir    = "/nfs/stak/users/kumarpra/.cabal/share/x86_64-linux-ghc-8.4.3/DPSemiring-0.1.0.0"
libexecdir = "/nfs/stak/users/kumarpra/.cabal/libexec/x86_64-linux-ghc-8.4.3/DPSemiring-0.1.0.0"
sysconfdir = "/nfs/stak/users/kumarpra/.cabal/etc"

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "DPSemiring_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "DPSemiring_libdir") (\_ -> return libdir)
getDynLibDir = catchIO (getEnv "DPSemiring_dynlibdir") (\_ -> return dynlibdir)
getDataDir = catchIO (getEnv "DPSemiring_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "DPSemiring_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "DPSemiring_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
