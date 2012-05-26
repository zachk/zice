module Paths_zice (
    version,
    getBinDir, getLibDir, getDataDir, getLibexecDir,
    getDataFileName
  ) where

import qualified Control.Exception as Exception
import Data.Version (Version(..))
import System.Environment (getEnv)
catchIO :: IO a -> (Exception.IOException -> IO a) -> IO a
catchIO = Exception.catch


version :: Version
version = Version {versionBranch = [0,0,1], versionTags = []}
bindir, libdir, datadir, libexecdir :: FilePath

bindir     = "/home/zachk/.cabal/bin"
libdir     = "/home/zachk/.cabal/lib/zice-0.0.1/ghc-7.4.1"
datadir    = "/home/zachk/.cabal/share/zice-0.0.1"
libexecdir = "/home/zachk/.cabal/libexec"

getBinDir, getLibDir, getDataDir, getLibexecDir :: IO FilePath
getBinDir = catchIO (getEnv "zice_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "zice_libdir") (\_ -> return libdir)
getDataDir = catchIO (getEnv "zice_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "zice_libexecdir") (\_ -> return libexecdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
