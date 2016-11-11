import System.Environment (getArgs)
import System.Exit (exitFailure)

import LexCPP  -- OR: CPP.Lex
import ParCPP  -- OR: CPP.Par
import ErrM    -- OR: CPP.ErrM

import TypeChecker
import Interpreter

-- driver

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
        Ok _ -> interpret tree

main :: IO ()
main = do
  args <- getArgs
  case args of
    [file] -> readFile file >>= check
    _      -> do
      putStrLn "Usage: lab2 <SourceFile>"
      exitFailure
