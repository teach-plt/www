module Main where

import Data.Maybe
import System.Environment
import System.FilePath
import System.Process

javaProg :: String
javaProg = "java"

className :: String
className = "lab2"

classpathSeparator :: Char
#if defined(mingw32_HOST_OS)
classpathSeparator = ';'
#else
classpathSeparator = ':'
#endif

(<:>) :: String -> String -> String
s <:> t = s ++ [classpathSeparator] ++ t

main = do
  args      <- getArgs
  dir       <- dropFileName <$> getExecutablePath
  classPath <- fromMaybe [] <$> lookupEnv "CLASSPATH"
  let cp      = dir <:> classPath
      cmdArgs = ["-cp",cp,className] ++ args
      cmdLine = showCommandForUser javaProg cmdArgs
  callProcess javaProg cmdArgs
