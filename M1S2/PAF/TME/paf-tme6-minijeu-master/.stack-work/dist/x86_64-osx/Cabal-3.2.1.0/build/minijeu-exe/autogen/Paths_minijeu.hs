{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
module Paths_minijeu (
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

bindir     = "/Users/yangchengyu/Downloads/PAF/TME/paf-tme6-minijeu-master/.stack-work/install/x86_64-osx/3122a9000d9df97b29b41f902165ba1022eb8d4369683beb2324c83e4e3103a2/8.10.3/bin"
libdir     = "/Users/yangchengyu/Downloads/PAF/TME/paf-tme6-minijeu-master/.stack-work/install/x86_64-osx/3122a9000d9df97b29b41f902165ba1022eb8d4369683beb2324c83e4e3103a2/8.10.3/lib/x86_64-osx-ghc-8.10.3/minijeu-0.1.0.0-AUc4WqPSgNJAjEfwgLevfA-minijeu-exe"
dynlibdir  = "/Users/yangchengyu/Downloads/PAF/TME/paf-tme6-minijeu-master/.stack-work/install/x86_64-osx/3122a9000d9df97b29b41f902165ba1022eb8d4369683beb2324c83e4e3103a2/8.10.3/lib/x86_64-osx-ghc-8.10.3"
datadir    = "/Users/yangchengyu/Downloads/PAF/TME/paf-tme6-minijeu-master/.stack-work/install/x86_64-osx/3122a9000d9df97b29b41f902165ba1022eb8d4369683beb2324c83e4e3103a2/8.10.3/share/x86_64-osx-ghc-8.10.3/minijeu-0.1.0.0"
libexecdir = "/Users/yangchengyu/Downloads/PAF/TME/paf-tme6-minijeu-master/.stack-work/install/x86_64-osx/3122a9000d9df97b29b41f902165ba1022eb8d4369683beb2324c83e4e3103a2/8.10.3/libexec/x86_64-osx-ghc-8.10.3/minijeu-0.1.0.0"
sysconfdir = "/Users/yangchengyu/Downloads/PAF/TME/paf-tme6-minijeu-master/.stack-work/install/x86_64-osx/3122a9000d9df97b29b41f902165ba1022eb8d4369683beb2324c83e4e3103a2/8.10.3/etc"

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "minijeu_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "minijeu_libdir") (\_ -> return libdir)
getDynLibDir = catchIO (getEnv "minijeu_dynlibdir") (\_ -> return dynlibdir)
getDataDir = catchIO (getEnv "minijeu_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "minijeu_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "minijeu_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
