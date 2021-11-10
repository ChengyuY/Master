{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
module Paths_validation (
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

bindir     = "/Users/yangchengyu/Downloads/PAF/TME/paf-tme7-validation-master/.stack-work/install/x86_64-osx/ebb04d62c4fe4f4a0db43c9bb948835007fea91853a98954c60c40d9834ad993/8.10.3/bin"
libdir     = "/Users/yangchengyu/Downloads/PAF/TME/paf-tme7-validation-master/.stack-work/install/x86_64-osx/ebb04d62c4fe4f4a0db43c9bb948835007fea91853a98954c60c40d9834ad993/8.10.3/lib/x86_64-osx-ghc-8.10.3/validation-0.1.0.0-HIQpitQ6l92LnorVk16F1j-validation-test"
dynlibdir  = "/Users/yangchengyu/Downloads/PAF/TME/paf-tme7-validation-master/.stack-work/install/x86_64-osx/ebb04d62c4fe4f4a0db43c9bb948835007fea91853a98954c60c40d9834ad993/8.10.3/lib/x86_64-osx-ghc-8.10.3"
datadir    = "/Users/yangchengyu/Downloads/PAF/TME/paf-tme7-validation-master/.stack-work/install/x86_64-osx/ebb04d62c4fe4f4a0db43c9bb948835007fea91853a98954c60c40d9834ad993/8.10.3/share/x86_64-osx-ghc-8.10.3/validation-0.1.0.0"
libexecdir = "/Users/yangchengyu/Downloads/PAF/TME/paf-tme7-validation-master/.stack-work/install/x86_64-osx/ebb04d62c4fe4f4a0db43c9bb948835007fea91853a98954c60c40d9834ad993/8.10.3/libexec/x86_64-osx-ghc-8.10.3/validation-0.1.0.0"
sysconfdir = "/Users/yangchengyu/Downloads/PAF/TME/paf-tme7-validation-master/.stack-work/install/x86_64-osx/ebb04d62c4fe4f4a0db43c9bb948835007fea91853a98954c60c40d9834ad993/8.10.3/etc"

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "validation_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "validation_libdir") (\_ -> return libdir)
getDynLibDir = catchIO (getEnv "validation_dynlibdir") (\_ -> return dynlibdir)
getDataDir = catchIO (getEnv "validation_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "validation_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "validation_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
