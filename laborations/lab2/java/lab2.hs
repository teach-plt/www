-- | OS-agnostic runner for PLT lab2 written in Haskell.

module Main where

import Data.Maybe
import System.Environment
import System.FilePath
import System.Process

javaProg :: String
javaProg = "java"

className :: String
className = "lab2"

-- | The class path separator is OS-specific:
--   ';' on Windows, ':' on linux/Unix/MacOSX.

classpathSeparator :: Char
#if defined(mingw32_HOST_OS)
classpathSeparator = ';'
#else
classpathSeparator = ':'
#endif

(<:>) :: String -> String -> String
s <:> t = s ++ [classpathSeparator] ++ t

main :: IO ()
main = do
  args      <- getArgs
  dir       <- dropFileName <$> getExecutablePath
  classPath <- fromMaybe "" <$> lookupEnv "CLASSPATH"
  let cp      = dir <:> classPath
      cmdArgs = ["-cp",cp,className] ++ args
      cmdLine = showCommandForUser javaProg cmdArgs
  callProcess javaProg cmdArgs
