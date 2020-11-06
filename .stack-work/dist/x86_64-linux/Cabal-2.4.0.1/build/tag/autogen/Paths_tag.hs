{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
module Paths_tag (
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

bindir     = "/home/francesco/pub/tag/.stack-work/install/x86_64-linux/9ecf73c7a477964af12dab29481ed693f7b03a77c20db389d2f9beb26f9a67f7/8.6.5/bin"
libdir     = "/home/francesco/pub/tag/.stack-work/install/x86_64-linux/9ecf73c7a477964af12dab29481ed693f7b03a77c20db389d2f9beb26f9a67f7/8.6.5/lib/x86_64-linux-ghc-8.6.5/tag-0.1.0.0-Fl0P8E1qeba8tS89r8GLlD-tag"
dynlibdir  = "/home/francesco/pub/tag/.stack-work/install/x86_64-linux/9ecf73c7a477964af12dab29481ed693f7b03a77c20db389d2f9beb26f9a67f7/8.6.5/lib/x86_64-linux-ghc-8.6.5"
datadir    = "/home/francesco/pub/tag/.stack-work/install/x86_64-linux/9ecf73c7a477964af12dab29481ed693f7b03a77c20db389d2f9beb26f9a67f7/8.6.5/share/x86_64-linux-ghc-8.6.5/tag-0.1.0.0"
libexecdir = "/home/francesco/pub/tag/.stack-work/install/x86_64-linux/9ecf73c7a477964af12dab29481ed693f7b03a77c20db389d2f9beb26f9a67f7/8.6.5/libexec/x86_64-linux-ghc-8.6.5/tag-0.1.0.0"
sysconfdir = "/home/francesco/pub/tag/.stack-work/install/x86_64-linux/9ecf73c7a477964af12dab29481ed693f7b03a77c20db389d2f9beb26f9a67f7/8.6.5/etc"

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "tag_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "tag_libdir") (\_ -> return libdir)
getDynLibDir = catchIO (getEnv "tag_dynlibdir") (\_ -> return dynlibdir)
getDataDir = catchIO (getEnv "tag_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "tag_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "tag_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
