{-# LANGUAGE PatternSynonyms #-}

import Control.Exception

import System.Environment (getArgs)
import System.Exit        (exitFailure)
import System.IO.Error    (isUserError, ioeGetErrorString)

import CMM.Par            (pProgram, myLexer)
import CMM.ErrM           (pattern Ok, pattern Bad)

import TypeChecker
import Interpreter

-- | Parse, type check, and interpret a program given by the @String@.

check :: String -> IO ()
check s = do
  case pProgram (myLexer s) of
    Bad err  -> do
      putStrLn "SYNTAX ERROR"
      putStrLn err
      exitFailure
    Ok  tree -> do
      case typecheck tree of
        Bad err -> do
          putStrLn "TYPE ERROR"
          putStrLn err
          exitFailure
        Ok _ -> catchJust (\e -> if isUserError e then Just (ioeGetErrorString e) else Nothing) (interpret tree) $
          \err -> do
            putStrLn "INTERPRETER ERROR"
            putStrLn err
            exitFailure

-- | Main: read file passed by only command line argument and call 'check'.

main :: IO ()
main = do
  args <- getArgs
  case args of
    [file] -> readFile file >>= check
    _      -> do
      putStrLn "Usage: lab2 <SourceFile>"
      exitFailure
