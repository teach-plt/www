module Main where

import System.Environment ( getArgs, getProgName )
import System.Exit        ( exitFailure, exitSuccess )
import System.FilePath    ( replaceExtension, takeBaseName )
import System.Process     ( callProcess )

import MiniJS.Par
import MiniJS.Print
import MiniJS.Abs
import MiniJS.ErrM

import Interpreter (interpret)
import TypeChecker (typeCheck)
import qualified AnnTypeChecker as A
import JVM         (mainClass)
import Compiler    (compile)

main :: IO ()
main = do
  args <- getArgs
  case args of
    ["--help"] -> usage
    []         -> getContents >>= run "mini"
    fs         -> mapM_ runFile fs

runFile :: FilePath -> IO ()
runFile f = do
  putStrLn $ "[File: " ++ f ++ " ]"
  readFile f >>= run f

run :: String -> String -> IO ()
run file content = do
  putStrLn "[Parsing...]"
  let ts = myLexer content
  case pProgram ts of

    Bad s    -> do
      putStrLn "Parse failed...\n"
      putStr "Tokens:"
      putStr $ show ts
      putStrLn content
      exitFailure

    Ok  prg -> do

      printProgram prg

      putStrLn "[Type checking...]"
      typeCheck prg `seq` return ()

      putStrLn "[Running...]"
      interpret prg

      putStrLn "[Type annotating...]"
      let prg' = A.typeCheck prg

      putStrLn "[Compiled to:]"
      let text = mainClass (takeBaseName file) $ compile prg'
      putStrLn text

      putStrLn "[Calling jasmin...]"
      runJasmin file text

      putStrLn "[Calling java]"
      runJava file

      exitSuccess

-- | Output abstract syntax and recovered concrete syntax.

printProgram :: Program -> IO ()
printProgram prg = do
  putStrLn ""
  putStrLn "[* Abstract Syntax *]"
  putStrLn $ show prg
  putStrLn ""
  putStrLn "[* Linearized tree *]"
  putStrLn $ printTree prg
  putStrLn ""

-- | Call jasmin to generate .class file.

runJasmin :: FilePath -> String -> IO ()
runJasmin file text = do
  let jfile = replaceExtension file "j"
  writeFile jfile text
  callProcess "jasmin" [jfile]

-- | Call java to execute .class file.

runJava :: FilePath -> IO ()
runJava file = callProcess "java" [takeBaseName file]

-- | Output usage information.

usage :: IO ()
usage = do
  putStrLn $ unlines
    [ "usage: Call with one of the following argument combinations:"
    , "  --help          Display this help message."
    , "  (no arguments)  Parse stdin."
    , "  (files)         Parse content of files."
    ]
  exitFailure
