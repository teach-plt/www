import Control.Monad.State

import System.IO
import System.FilePath.Posix
import System.Environment
import System.Exit

import Fun.Lex
import Fun.Par
import Fun.ErrM

import Interpreter

run :: Strategy -> String -> IO ()
run strategy s = case pProgram (myLexer s) of
  Bad err -> do
    putStrLn "SYNTAX ERROR"
    putStrLn err
    exitFailure
  Ok prg -> do
      case interpret strategy prg of
        Bad err -> do
          putStrLn "INTERPRETER ERROR"
          putStrLn err
          exitFailure
        Ok i -> do
          putStrLn $ show i
          hPutStrLn stderr "OK"
          exitSuccess

data CmdLine = CmdLine
  { strategy :: Strategy
  , fileName :: FilePath
  }

initCmdLine :: CmdLine
initCmdLine = CmdLine
  { strategy = CallByValue
  , fileName = ""
  }

usage :: IO ()
usage = do
  putStrLn "Usage: lab4 [-n|-v] <SourceFile>"
  exitFailure

parseArg :: String -> StateT CmdLine IO ()
parseArg s
  | s == "-n" = modify $ \ o -> o { strategy = CallByName  }
  | s == "-v" = modify $ \ o -> o { strategy = CallByValue }
  | otherwise = do
      old <- gets fileName
      if null old then modify $ \ o -> o { fileName = s }
      else lift $ usage

main :: IO ()
main = do
  args <- getArgs
  o <- mapM_ parseArg args `execStateT` initCmdLine
  let file = fileName o
  when (null file) usage
  run (strategy o) =<< readFile file
