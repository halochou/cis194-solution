module Paths_week12 (
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

bindir     = "/Users/halo/code/cis194/week12/.stack-work/install/x86_64-osx/lts-6.6/7.10.3/bin"
libdir     = "/Users/halo/code/cis194/week12/.stack-work/install/x86_64-osx/lts-6.6/7.10.3/lib/x86_64-osx-ghc-7.10.3/week12-0.1.0.0-GvFTF9WK6lF5y5rctsHK1g"
datadir    = "/Users/halo/code/cis194/week12/.stack-work/install/x86_64-osx/lts-6.6/7.10.3/share/x86_64-osx-ghc-7.10.3/week12-0.1.0.0"
libexecdir = "/Users/halo/code/cis194/week12/.stack-work/install/x86_64-osx/lts-6.6/7.10.3/libexec"
sysconfdir = "/Users/halo/code/cis194/week12/.stack-work/install/x86_64-osx/lts-6.6/7.10.3/etc"

getBinDir, getLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "week12_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "week12_libdir") (\_ -> return libdir)
getDataDir = catchIO (getEnv "week12_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "week12_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "week12_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
