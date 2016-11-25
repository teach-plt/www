{-# LANGUAGE CPP #-}
{-# LANGUAGE TupleSections #-}

-- GHC needs -threaded

import Control.Concurrent
import Control.Monad

import Data.Char
import Data.Functor
import Data.IORef
import Data.List
import Data.Maybe

import System.Directory
import System.Environment
import System.Exit
import System.IO
import System.Process
import System.IO.Unsafe
import System.FilePath.Posix (takeBaseName, takeDirectory)

#if __GLASGOW_HASKELL__ >= 706
-- needed in GHC 7.6
import Control.Exception
readFileIfExists :: FilePath -> IO String
readFileIfExists f = catch (readFile f) exceptionHandler
   where exceptionHandler :: IOException -> IO String
         exceptionHandler _ = return ""
#else
-- whereas in GHC 7.4
readFileIfExists :: FilePath -> IO String
readFileIfExists f = catch (readFile f) (\_ -> return "")
#endif

--
-- * Main
--

main :: IO ()
main = mainOpts =<< parseArgs =<< getArgs

-- | Filter out and process options, return the rest.
parseArgs :: [String] -> IO [String]
parseArgs args = do
  let isOpt ('-':_) = True
      isOpt _       = False
  let (opts, rest) = partition isOpt args
  processOpts opts
  when (null rest) $ usage
  return rest

processOpts :: [String] -> IO ()
processOpts = mapM_ $ \ arg -> case arg of
  "-debug"  -> writeIORef doDebug True
  "--debug" -> writeIORef doDebug True
  _ -> usage

usage :: IO a
usage = do
  hPutStrLn stderr "Usage: progs-test-lab3 <interpreter code directory> [<test case directory>]*"
  exitFailure

mainOpts :: [FilePath] -> IO ()
mainOpts (progdir : testdirs) = do
  putStrLn $ "This is the (reduced) test program for Programming Languages Lab 3"
  runCommandNoFail_ "rm -f */*.j */*.class" ""
  runMake progdir
  good <- runTests progdir testdirs
  putStrLn ""
  putStrLn "------------------------------------------------------------"
  report "Good programs: " good

--
-- * Test driver
--

-- | Executable name
executable_name = "lab3"

-- | Run "make" in given directory.
runMake :: FilePath -> IO ()
runMake dir = do
  checkDirectoryExists dir
  runCommandNoFail_ ("make -C " ++ quote dir) ""

-- | Run test on all ".cc" files in given directories (default "good").
runTests :: FilePath -> [FilePath] -> IO [(FilePath,Bool)]
runTests dir testdirs = do
  let prog = joinPath [dir, executable_name]
  checkFileExists prog
  let dirs = if null testdirs then ["good"] else testdirs
  files <- concat <$> mapM listCCFiles dirs
  mapM (\ f -> (f,) <$> testBackendProg prog f) files

-- | Test given program on given test file.
testBackendProg
  :: FilePath  -- ^ Program (lab3).
  -> FilePath  -- ^ Test file, e.g., good/good01.cc
  -> IO Bool   -- ^ Test successful?
testBackendProg prog f = do
  input  <- readFileIfExists (f ++ ".input")
  output <- readFileIfExists (f ++ ".output")

  -- Running prog on f should generate file f.class
  putStrLn $ "Running " ++ f ++ "..."
  _ <- runCommandStrWait (prog ++ " " ++ f) ""

  -- Run code
  let c = "java -noverify -cp .:" ++ takeDirectory f ++ " " ++ takeBaseName f
  (out,err,s) <- runCommandStrWait c input
  debug $ "Exit code: " ++ show s
  if out == output then return True else do
    reportError c "invalid output" f input out err
    putStrLn "Expected output:"
    putStrLn $ color blue $ output
    return False

-- | Return all files with extension ".cc" in given directory.
listCCFiles :: FilePath -> IO [FilePath]
listCCFiles dir =
  liftM (map (\f -> joinPath [dir,f]) . sort . filter ((=="cc") . getExt)) $
    getDirectoryContents dir

--
-- * Debugging
--

-- | Is debugging on?
{-# NOINLINE doDebug #-}
doDebug :: IORef Bool
doDebug = unsafePerformIO $ newIORef False

-- | Print debug message if debugging is on.
debug :: String -> IO ()
debug s = do
  d <- readIORef doDebug
  when d $ putStrLn s

--
-- * Path name utilities
--

getExt :: FilePath -> String
getExt = reverse . takeWhile (/='.') . reverse

joinPath :: [String] -> FilePath
joinPath = concat . intersperse [pathSep]

pathSep :: Char
#if defined(mingw32_HOST_OS)
pathSep = '\\'
#else
pathSep = '/'
#endif

quote :: FilePath -> FilePath
quote p = "'" ++ concatMap f p ++ "'"
  where
    f '\'' = "\\'"
    f c = [c]

--
-- * Terminal output colors
--

type Color = Int

color :: Color -> String -> String
color c s = fgcol c ++ s ++ normal

highlight = "\ESC[7m"
bold      = "\ESC[1m"
underline = "\ESC[4m"
normal    = "\ESC[0m"
fgcol col = "\ESC[0" ++ show (30+col) ++ "m"
bgcol col = "\ESC[0" ++ show (40+col) ++ "m"

red, green, blue, black :: Color
black = 0
red   = 1
green = 2
blue  = 6

--
-- * Various versions of runCommand
--

runCommandStr
  :: String                           -- ^ command
  -> String                           -- ^ stdin data
  -> IO (String,String,ProcessHandle) -- ^ stdout, stderr, process
runCommandStr c inStr = do
  outVar <- newEmptyMVar
  errVar <- newEmptyMVar
  (pin,pout,perr,p) <- runInteractiveCommand c

  forkIO $ do
    debug "Writing input..."
    hPutStr pin inStr
    hClose pin
    debug "Wrote input."

  forkIO $ do
    debug "Reading output..."
    s <- hGetContents pout
    putMVar outVar s
    debug "Read output."

  forkIO $ do
    debug "Reading error..."
    s <- hGetContents perr
    putMVar errVar s
    debug "Read error."

  out <- takeMVar outVar
  err <- takeMVar errVar
  return (out,err,p)


runCommandStrWait
  :: String                      -- ^ command
  -> String                      -- ^ stdin data
  -> IO (String,String,ExitCode) -- ^ stdout, stderr, process exit status
runCommandStrWait c inStr = do
  debug $ "Running " ++ c
  (out,err,p) <- runCommandStr c inStr
  s <- waitForProcess p
  debug $ "Standard output:\n" ++ out
  debug $ "Standard error:\n" ++ err
  return (out,err,s)

runCommandNoFail_
  :: String   -- ^ Command
  -> FilePath -- ^ Input file
  -> IO ()
runCommandNoFail_ c f = runCommandNoFail c f >> return ()

runCommandNoFail
  :: String             -- ^ Command
  -> FilePath           -- ^ Input file
  -> IO (String,String) -- ^ stdout and stderr
runCommandNoFail e f = do
  let c = e ++ " " ++ f
  hPutStrLn stderr $ "Running " ++ c ++ "..."
  (out,err,s) <- runCommandStrWait c ""
  case s of
    ExitFailure x -> do
      reportError e ("with status " ++ show x) f "" out err
      exitFailure
    ExitSuccess -> return (out,err)

--
-- * Checking files and directories
--

checkFileExists :: FilePath -> IO ()
checkFileExists f = do
  e <- doesFileExist f
  unless e $ do
    putStrLn $ color red $ quote f ++ " is not an existing file."
    exitFailure

checkDirectoryExists :: FilePath -> IO ()
checkDirectoryExists f = do
  e <- doesDirectoryExist f
  unless e $ do
    putStrLn $ color red $ quote f ++ " is not an existing directory."
    exitFailure

--
-- * Error reporting and output checking
--

reportErrorColor
  :: Color
  -> String   -- ^ command that failed
  -> String   -- ^ how it failed
  -> FilePath -- ^ source file
  -> String   -- ^ given input
  -> String   -- ^ stdout output
  -> String   -- ^ stderr output
  -> IO ()
reportErrorColor col c m f i o e = do
  putStrLn $ color col $ c ++ " failed: " ++ m
  unless (null f) $ prFile f
  unless (null i) $ do
    putStrLn "Given this input:"
    putStrLn $ color blue $ i
  unless (null o) $ do
    putStrLn "It printed this to standard output:"
    putStrLn $ color blue $ o
  unless (null e) $ do
    putStrLn "It printed this to standard error:"
    putStrLn $ color blue $ e

reportError
  :: String   -- ^ command that failed
  -> String   -- ^ how it failed
  -> FilePath -- ^ source file
  -> String   -- ^ given input
  -> String   -- ^ stdout output
  -> String   -- ^ stderr output
  -> IO ()
reportError = reportErrorColor red

prFile :: FilePath -> IO ()
prFile f = do
  e <- doesFileExist f
  when e $ do
    putStrLn $ "For input file " ++ f ++ ":"
    putStrLn $ "---------------- begin " ++ f ++ " ------------------"
    s <- readFile f
    putStrLn $ color green s
    putStrLn $ "----------------- end " ++ f ++ " -------------------"

-- | Report how many tests passed and which tests failed (if any).
report :: String -> [(String,Bool)] -> IO ()
report n rs = do
  let (pass, fail) = partition snd rs
  let (p,t) = (length pass, length rs)
      c     = if p == t then green else red
  putStrLn $ color c $ n ++ "passed " ++ show p ++ " of " ++ show t ++ " tests"
  unless (null fail) $
    mapM_ (putStrLn . color red) $ "Failed tests:" : map fst fail
