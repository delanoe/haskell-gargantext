module Paths_hastext (
    version,
    getBinDir, getLibDir, getDataDir, getLibexecDir,
    getDataFileName, getSysconfDir
  ) where

import qualified Control.Exception as Exception
import Data.Version (Version(..))
import System.Environment (getEnv)
import Prelude

catchIO :: IO a -> (Exception.IOException -> IO a) -> IO a
catchIO = Exception.catch

version :: Version
version = Version [0,1,0,0] []
bindir, libdir, datadir, libexecdir, sysconfdir :: FilePath

bindir     = "/home/alexandre/.cabal/bin"
libdir     = "/home/alexandre/.cabal/lib/x86_64-linux-ghc-7.8.3/hastext-0.1.0.0"
datadir    = "/home/alexandre/.cabal/share/x86_64-linux-ghc-7.8.3/hastext-0.1.0.0"
libexecdir = "/home/alexandre/.cabal/libexec"
sysconfdir = "/home/alexandre/.cabal/etc"

getBinDir, getLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "hastext_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "hastext_libdir") (\_ -> return libdir)
getDataDir = catchIO (getEnv "hastext_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "hastext_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "hastext_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
