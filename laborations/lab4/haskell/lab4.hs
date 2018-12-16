import Control.Monad        (forM_, when)
import Control.Monad.State  (StateT, execStateT, gets, modify, liftIO)
import Control.Monad.Except (runExcept)

import System.IO            (hPutStrLn, readFile, stderr)
import System.Environment   (getArgs)
import System.Exit          (exitFailure, exitSuccess)

import Fun.Abs              (Program)
import Fun.Par              (pProgram, myLexer)
import Fun.Print            (printTree)
import Fun.ErrM             (Err(..))

import Interpreter          (Strategy(..), interpret)

-- | Entry point.

main :: IO ()
main = do
  args <- getArgs
  CmdLine strategy file <- mapM_ parseArg args `execStateT` initCmdLine
  when (null file) usage
  run strategy =<< readFile file

-- | Main pipeline.

run :: Strategy -> String -> IO ()
run strategy s = eval strategy =<< parse s

-- | Parse.

parse :: String -> IO Program
parse s = case pProgram (myLexer s) of
  Bad err -> do
    putStrLn "SYNTAX ERROR"
    putStrLn err
    exitFailure
  Ok prg -> return prg


-- | Interpret in call-by-value or call-by-name.

eval :: Strategy -> Program -> IO ()
eval strategy prg = do
  case runExcept $ interpret strategy prg of
    Left err -> do
      putStrLn "INTERPRETER ERROR"
      putStrLn err
      exitFailure
    Right i -> do
      putStrLn $ show i
      hPutStrLn stderr "OK"
      exitSuccess

-- * Command-line parsing

-- | State of the command line parser.

data CmdLine = CmdLine
  { strategy :: Strategy
  , fileName :: FilePath
  }

-- | Initial state: 'CallByValue' is default.

initCmdLine :: CmdLine
initCmdLine = CmdLine
  { strategy = CallByValue
  , fileName = ""
  }

-- | Parse a single command line argument.

parseArg :: String -> StateT CmdLine IO ()
parseArg s
  | s == "-n" = modify $ \ o -> o { strategy = CallByName }
  | s == "-v" = modify $ \ o -> o { strategy = CallByValue }
  | otherwise = do
      old <- gets fileName
      if null old then modify $ \ o -> o { fileName = s }
      else liftIO $ usage

-- | Print usage information and exit.

usage :: IO ()
usage = do
  putStrLn "Usage: lab4 [-n|-v] <SourceFile>"
  exitFailure
