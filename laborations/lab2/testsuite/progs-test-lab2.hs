{-# LANGUAGE CPP #-}

-- GHC needs -threaded

import Control.Exception
import Control.Monad

import Data.Char
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
executable_name = "lab2"

{-# NOINLINE doDebug #-}
doDebug :: IORef Bool
doDebug = unsafePerformIO $ newIORef False

-- | Whether to run make
{-# NOINLINE doMake  #-}
doMake :: IORef Bool
doMake = unsafePerformIO $ newIORef True

debug :: String -> IO ()
debug s = do d <- readIORef doDebug
             if d then putStrLn s else return ()


listGoodProgs = listCCFiles "good"

listBadProgs = listCCFiles "bad"

listCCFiles dir =
    liftM (map (\f -> joinPath [dir,f]) . sort . filter ((==".cc") . takeExtension)) $ listDirectory dir


welcome :: IO ()
welcome = do putStrLn $ "This is the test program for Programming Languages Lab 2"


runMake :: FilePath -> IO ()
runMake dir = do checkDirectoryExists dir
                 runPrgNoFail_ "make" ["-C"] dir

runTests :: FilePath -> IO ([Bool],[Bool])
runTests dir =
    do let prog = joinPath [dir,executable_name]
       checkFileExists prog
       goodProgs <- listGoodProgs
       badProgs  <- listBadProgs
       good <- mapM (testBackendProg prog) goodProgs
       bad  <- mapM (testBadProgram prog) badProgs
       return (good,bad)

testBackendProg :: FilePath -> FilePath -> IO Bool
testBackendProg prog f =
    do input  <- readFileIfExists (f++".input")
       output <- readFileIfExists (f++".output")
       putStrLn $ "Running " ++ f ++ "..."
       (s,out,err) <- readProcessWithExitCode prog [f] input
       debug $ "Exit code: " ++ show s
       -- Try to work around line ending problem
       let removeCR = filter (/= '\r')
       if trim (removeCR out) == trim (removeCR output)
         then if (trim (removeCR err) /= "")
              then reportError prog "unexpected output on stderr" f input out err >>
                   return False
              else return True
         else do reportError prog "invalid output" f input out err
                 putStrLn "Expected output:"
                 putStrLn $ color blue $ output
                 return False

testBadProgram :: FilePath -> FilePath -> IO Bool
testBadProgram prog f = do
  input  <- readFileIfExists (f++".input")
  --output <- readFileIfExists (f++".output")
  putStrLn $ "Running " ++ f ++ "..."
  (s,out,err) <- readProcessWithExitCode prog [f] input
  debug $ "Exit code: " ++ show s
  if "TYPE ERROR" `isPrefixOf` out then return True else
    if "SYNTAX ERROR" `isPrefixOf` out then return True else do
      reportError prog "Bad program passed type checking" f "" out err
      return False

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
  unless (length rest == 1) $ usage
  return rest

processOpts :: [String] -> IO ()
processOpts = mapM_ $ \ arg -> case arg of
  "--debug" -> writeIORef doDebug True
  "--no-make" -> writeIORef doMake False
  _ -> usage

usage :: IO a
usage = do
  hPutStrLn stderr "Usage: progs-test-lab2 [--debug] interpreter_code_directory"
  exitFailure

mainOpts :: [FilePath] -> IO ()
mainOpts [dir] =
    do welcome
       domake <- readIORef doMake
       when domake $ runMake dir
       (good,bad) <- runTests dir
       putStrLn ""
       putStrLn "------------------------------------------------------------"
       report "Good programs: " good
       report "Bad programs:  " bad

--
-- * Utilities
--

trim :: String -> String
trim = f . f
  where f = reverse . dropWhile isSpace

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
red = 1
green = 2
blue = 6

--
-- * Run programs
--

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
checkFileExists f =
    do e <- doesFileExist f
       when (not e) $ do putStrLn $ color red $ quote f ++ " is not an existing file."
                         exitFailure

checkDirectoryExists :: FilePath -> IO ()
checkDirectoryExists f =
    do e <- doesDirectoryExist f
       when (not e) $ do putStrLn $ color red $ quote f ++ " is not an existing directory."
                         exitFailure

--
-- * Error reporting and output checking
--

reportErrorColor :: Color
                 -> String -- ^ command that failed
                 -> String -- ^ how it failed
                 -> FilePath -- ^ source file
                 -> String -- ^ given input
                 -> String -- ^ stdout output
                 -> String -- ^ stderr output
                 -> IO ()
reportErrorColor col c m f i o e =
    do
    putStrLn $ color col $ c ++ " failed: " ++ m
    when (not (null f)) $ prFile f
    when (not (null i)) $ do
                          putStrLn "Given this input:"
                          putStrLn $ color blue $ i
    when (not (null o)) $ do
                          putStrLn "It printed this to standard output:"
                          putStrLn $ color blue $ o
    when (not (null e)) $ do
                          putStrLn "It printed this to standard error:"
                          putStrLn $ color blue $ e

reportError :: String -- ^ command that failed
            -> String -- ^ how it failed
            -> FilePath -- ^ source file
            -> String -- ^ given input
            -> String -- ^ stdout output
            -> String -- ^ stderr output
            -> IO ()
reportError = reportErrorColor red

prFile :: FilePath -> IO ()
prFile f = do
           e <- doesFileExist f
           when e $ do putStrLn $ "For input file " ++ f ++ ":"
                       putStrLn $ "---------------- begin " ++ f ++ " ------------------"
                       s <- readFile f
                       putStrLn $ color green s
                       putStrLn $ "----------------- end " ++ f ++ " -------------------"


-- | Report how many tests passed.
report :: String -> [Bool] -> IO ()
report n rs =
  do let (p,t) = (length (filter id rs), length rs)
         c = if p == t then green else red
     putStrLn $ color c $
              n ++ "passed " ++ show p ++ " of " ++ show t ++ " tests"
