{-# LANGUAGE CPP #-}

-- | OS-agnostic runner for Java solutions of PLT labs.

module Main where

import Data.Maybe
import System.Environment
import System.FilePath
import System.Process

-- | Name of the JVM.

javaProg :: String
javaProg = "java"

-- | The base name of the class file to run is assumed to have
--   the same name as this runner.

getClassName :: IO String
getClassName = dropExtension <$> getProgName

-- Should this not work, then one may give the name of the class directly, e.g.
-- getClassName = return "lab2"

-- | The class path separator is OS-specific:
--   ';' on Windows, ':' on linux/Unix/MacOSX.

classpathSeparator :: Char
#if defined(mingw32_HOST_OS)
classpathSeparator = ';'
#else
classpathSeparator = ':'
#endif

-- | Concatenating class pathes.

(<:>) :: String -> String -> String
s <:> t = s ++ [classpathSeparator] ++ t

-- | Run the class file with class path extended by the directory
--   of the class file.

main :: IO ()
main = do
  args      <- getArgs
  dir       <- dropFileName <$> getExecutablePath
  className <- getClassName
  classPath <- fromMaybe "" <$> lookupEnv "CLASSPATH"
  let cp      = dir <:> classPath
      cmdArgs = ["-Xss80m","-cp",cp,className] ++ args
      -- cmdLine = showCommandForUser javaProg cmdArgs
  callProcess javaProg cmdArgs
