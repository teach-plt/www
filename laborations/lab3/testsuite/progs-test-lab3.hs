{-# LANGUAGE CPP #-}
{-# LANGUAGE TupleSections #-}

-- GHC needs -threaded

import Control.Exception
import Control.Monad

import Data.IORef
import Data.List

import System.Directory
import System.Environment
import System.FilePath
import System.Exit
import System.IO
import System.Process
import System.IO.Unsafe

-- Executable name
-- You might have to add .exe here if you are using Windows
executable_name = "lab3"

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
  "--debug" -> writeIORef doDebug True
  "--doubles" -> writeIORef includeDoubleTests True
  "--no-make" -> writeIORef doMake False
  _ -> usage

usage :: IO a
usage = do
  hPutStrLn stderr "Usage: progs-test-lab3 [--debug] [--doubles]"
  hPutStrLn stderr "           interpreter_code_directory [test_case_directory ...]"
  exitFailure

mainOpts :: [FilePath] -> IO ()
mainOpts (progdir : dirs) = do
  putStrLn "This is the test program for Programming Languages Lab 3"
  doubles <- readIORef includeDoubleTests
  unless doubles $ putStrLn "Make sure to include the --doubles flag if you also want to test programs including doubles."
  let testdirs = if null dirs then ["good", "dir-for-path-test/one-more-dir"] else dirs
  -- Cleanup files from old runs
  forM_ testdirs (flip cleanDirectory [".j", ".class"])
  domake <- readIORef doMake
  when domake $ runMake progdir
  good <- runTests progdir testdirs
  putStrLn ""
  putStrLn "------------------------------------------------------------"
  report "Good programs: " good

--
-- * Test driver
--

-- | Whether to run tests involving doubles
{-# NOINLINE includeDoubleTests  #-}
includeDoubleTests :: IORef Bool
includeDoubleTests = unsafePerformIO $ newIORef False

-- | Whether to run make
{-# NOINLINE doMake  #-}
doMake :: IORef Bool
doMake = unsafePerformIO $ newIORef True

-- | Run "make" in given directory.
runMake :: FilePath -> IO ()
runMake dir = do
  checkDirectoryExists dir
  runPrgNoFail_ "make" ["-C"] dir

-- | Run test on all ".cc" files in given directories (default "good").
runTests :: FilePath -> [FilePath] -> IO [(FilePath,Bool)]
runTests dir testdirs = do
  let prog = joinPath [dir, executable_name]
  checkFileExists prog
  files <- concat <$> mapM listCCFiles testdirs
  mapM (\ f -> (f,) <$> testBackendProg prog f) files

-- | Test given program on given test file.
testBackendProg
  :: FilePath  -- ^ Program
  -> FilePath  -- ^ Test file
  -> IO Bool
testBackendProg prog f = do
  input  <- readFileIfExists (f ++ ".input")
  output <- readFileIfExists (f ++ ".output")

  -- Running prog on f should generate file f.class
  putStrLn $ "Running " ++ f ++ "..."
  (compilerRet, compilerOut, compilerErr) <- readProcessWithExitCode prog [f] ""
  if isExitFailure compilerRet then do
    reportError prog "non-zero exit code" f input compilerOut compilerErr
    return False
  else do
    let expectedJavaClassFilePath = replaceExtension f ".class"
    javaClassFileCreated <- doesFileExist expectedJavaClassFilePath
    if javaClassFileCreated then do
      -- Run code
      -- A. Abel, 2018-11-26: put test file directory first in classpath.
      -- This avoids problems if there are stale .class files
      -- in the directory indicated by "." (the current directory).
      let classpath = takeDirectory f ++ [classpathSep, '.']
      -- let classpath = ['.', classpathSep] ++ takeDirectory f
      (javaRet, javaOut, javaErr) <- readProcessWithExitCode "java" ["-cp", classpath, takeBaseName f] input
      if isExitFailure javaRet then do
        reportError "java" "non-zero exit code" f input javaOut javaErr
        return False
      else do
        if javaOut == output then
          return True
        else do
          reportError "java" "invalid output" f input javaOut javaErr
          putStrLn "Expected output:"
          putStrLn $ color blue $ output
          return False
    else do
      reportError prog ("did not find any Java class file at \"" ++ expectedJavaClassFilePath ++ "\" (note that the output Java class file must be written to same directory as the input C++ file)") f input compilerOut compilerErr
      return False

-- | Return all files with extension ".cc" in given directory.
listCCFiles :: FilePath -> IO [FilePath]
listCCFiles dir = do
  doubles <- readIORef includeDoubleTests
  liftM (map (\f -> joinPath [dir,f]) . sort . filter (doublesFilter doubles) . filter ((==".cc") . takeExtension)) $
    listDirectory dir
  where doublesFilter doubles filename = doubles || not (isPrefixOf "double__" filename)

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
-- * Utilities
--

cleanDirectory :: FilePath -> [String] -> IO ()
cleanDirectory path exts = listDirectory path >>=
                           mapM_ (\f -> do let pathf = path </> f
                                           isFile <- doesFileExist pathf
                                           when (takeExtension f `elem` exts && isFile) $ removeFile pathf)

classpathSep :: Char
#if defined(mingw32_HOST_OS)
classpathSep = ';'
#else
classpathSep = ':'
#endif

quote :: FilePath -> FilePath
quote p = "'" ++ concatMap f p ++ "'"
  where
    f '\'' = "\\'"
    f c = [c]

readFileIfExists :: FilePath -> IO String
readFileIfExists f = catch (readFile f) exceptionHandler
   where exceptionHandler :: IOException -> IO String
         exceptionHandler _ = return ""

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
-- * Run programs
--

isExitFailure :: ExitCode -> Bool
isExitFailure ExitSuccess = False
isExitFailure ExitFailure{} = True

runPrgNoFail_ :: FilePath -- ^ Executable
              -> [String] -- ^ Flags

              -> FilePath -- ^ Filename
              -> IO ()
runPrgNoFail_ exe flags file = runPrgNoFail exe flags file >> return ()

runPrgNoFail :: FilePath -- ^ Executable
             -> [String] -- ^ Flag
             -> FilePath -- ^ Filename
             -> IO (String,String) -- ^ stdout and stderr
runPrgNoFail exe flags file = do
  let c = showCommandForUser exe (flags ++ [file])
  hPutStrLn stderr $ "Running " ++ c ++ "..."
  (s,out,err) <- readProcessWithExitCode exe (flags ++ [file]) ""
  case s of
    ExitFailure x -> do
      reportError exe ("with status " ++ show x) file "" out err
      exitFailure
    ExitSuccess -> do
      debug $ "Standard output:\n" ++ out
      debug $ "Standard error:\n" ++ err
      return (out,err)

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
