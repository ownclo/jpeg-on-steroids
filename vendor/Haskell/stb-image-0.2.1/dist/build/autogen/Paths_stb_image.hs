module Paths_stb_image (
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
version = Version {versionBranch = [0,2,1], versionTags = []}
bindir, libdir, datadir, libexecdir :: FilePath

bindir     = "/home/ownclo/.cabal/bin"
libdir     = "/home/ownclo/.cabal/lib/stb-image-0.2.1/ghc-7.4.1"
datadir    = "/home/ownclo/.cabal/share/stb-image-0.2.1"
libexecdir = "/home/ownclo/.cabal/libexec"

getBinDir, getLibDir, getDataDir, getLibexecDir :: IO FilePath
getBinDir = catchIO (getEnv "stb_image_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "stb_image_libdir") (\_ -> return libdir)
getDataDir = catchIO (getEnv "stb_image_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "stb_image_libexecdir") (\_ -> return libexecdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
