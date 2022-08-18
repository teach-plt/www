{-# LANGUAGE LambdaCase #-}

-- | lab2: Interpreter and type checker for C--.

import Control.Exception

import System.Environment (getArgs)
import System.Exit        (exitFailure)
import System.IO.Error    (isUserError, ioeGetErrorString)

import CMM.Par            (pProgram, myLexer)

import TypeChecker        (typecheck)
import Interpreter        (interpret)

-- | Parse, type check, and interpret a program given by the @String@.

check :: String -> IO ()
check s = do
  case pProgram (myLexer s) of
    Left err  -> do
      putStrLn "SYNTAX ERROR"
      putStrLn err
      exitFailure
    Right tree -> do
      case typecheck tree of
        Left err -> do
          putStrLn "TYPE ERROR"
          putStrLn err
          exitFailure
        Right tree' -> catchUserError (interpret tree') $ \ err -> do
          putStrLn "INTERPRETER ERROR"
          putStrLn err
          exitFailure

  where
  catchUserError :: IO a -> (String -> IO a) -> IO a
  catchUserError = catchJust $ \ exc ->
    if isUserError exc then Just (ioeGetErrorString exc) else Nothing

-- | Main: read file passed by only command line argument and call 'check'.

main :: IO ()
main = do
  getArgs >>= \case
    [file] -> readFile file >>= check
    _      -> do
      putStrLn "Usage: lab2 <SourceFile>"
      exitFailure
