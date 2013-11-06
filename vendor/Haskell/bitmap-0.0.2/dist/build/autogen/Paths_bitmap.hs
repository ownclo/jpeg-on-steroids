module Paths_bitmap (
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
version = Version {versionBranch = [0,0,2], versionTags = []}
bindir, libdir, datadir, libexecdir :: FilePath

bindir     = "/home/ownclo/.cabal/bin"
libdir     = "/home/ownclo/.cabal/lib/bitmap-0.0.2/ghc-7.4.1"
datadir    = "/home/ownclo/.cabal/share/bitmap-0.0.2"
libexecdir = "/home/ownclo/.cabal/libexec"

getBinDir, getLibDir, getDataDir, getLibexecDir :: IO FilePath
getBinDir = catchIO (getEnv "bitmap_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "bitmap_libdir") (\_ -> return libdir)
getDataDir = catchIO (getEnv "bitmap_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "bitmap_libexecdir") (\_ -> return libexecdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
