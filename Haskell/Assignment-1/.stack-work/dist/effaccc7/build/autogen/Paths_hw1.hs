{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
#if __GLASGOW_HASKELL__ >= 810
{-# OPTIONS_GHC -Wno-prepositive-qualified-module #-}
#endif
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
{-# OPTIONS_GHC -w #-}
module Paths_hw1 (
    version,
    getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir,
    getDataFileName, getSysconfDir
  ) where


import qualified Control.Exception as Exception
import qualified Data.List as List
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

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir `joinFileName` name)

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath




bindir, libdir, dynlibdir, datadir, libexecdir, sysconfdir :: FilePath
bindir     = "C:\\Users\\azant\\Documents\\School\\CptS_355\\Assigment_1\\Haskell\\.stack-work\\install\\a1547a0b\\bin"
libdir     = "C:\\Users\\azant\\Documents\\School\\CptS_355\\Assigment_1\\Haskell\\.stack-work\\install\\a1547a0b\\lib\\x86_64-windows-ghc-9.6.6\\hw1-0.1.0.0-HkaSruB0Sj0EVbc1BxqHGq"
dynlibdir  = "C:\\Users\\azant\\Documents\\School\\CptS_355\\Assigment_1\\Haskell\\.stack-work\\install\\a1547a0b\\lib\\x86_64-windows-ghc-9.6.6"
datadir    = "C:\\Users\\azant\\Documents\\School\\CptS_355\\Assigment_1\\Haskell\\.stack-work\\install\\a1547a0b\\share\\x86_64-windows-ghc-9.6.6\\hw1-0.1.0.0"
libexecdir = "C:\\Users\\azant\\Documents\\School\\CptS_355\\Assigment_1\\Haskell\\.stack-work\\install\\a1547a0b\\libexec\\x86_64-windows-ghc-9.6.6\\hw1-0.1.0.0"
sysconfdir = "C:\\Users\\azant\\Documents\\School\\CptS_355\\Assigment_1\\Haskell\\.stack-work\\install\\a1547a0b\\etc"

getBinDir     = catchIO (getEnv "hw1_bindir")     (\_ -> return bindir)
getLibDir     = catchIO (getEnv "hw1_libdir")     (\_ -> return libdir)
getDynLibDir  = catchIO (getEnv "hw1_dynlibdir")  (\_ -> return dynlibdir)
getDataDir    = catchIO (getEnv "hw1_datadir")    (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "hw1_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "hw1_sysconfdir") (\_ -> return sysconfdir)



joinFileName :: String -> String -> FilePath
joinFileName ""  fname = fname
joinFileName "." fname = fname
joinFileName dir ""    = dir
joinFileName dir fname
  | isPathSeparator (List.last dir) = dir ++ fname
  | otherwise                       = dir ++ pathSeparator : fname

pathSeparator :: Char
pathSeparator = '\\'

isPathSeparator :: Char -> Bool
isPathSeparator c = c == '/' || c == '\\'
