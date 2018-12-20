{-# LANGUAGE CPP #-}

-- GHC needs -threaded

import Control.Applicative
import Control.Exception
import Control.Monad

import Data.Char
import Data.IORef
import Data.List
import Data.Maybe

import System.Console.GetOpt
import System.Directory
import System.Environment
import System.FilePath
import System.Exit
import System.IO
import System.Process
import System.IO.Unsafe

-- Executable name
executable_name :: FilePath
-- You might have to add or remove .exe here if you are using Windows
executable_name = "lab2" <.> exeExtension

sequenceTuple :: Applicative f => (f a,f b) -> f (a,b)
sequenceTuple = uncurry $ liftA2 (,)

concatMapM :: Monad m => (a -> m [b]) -> [a] -> m [b]
concatMapM f = fmap concat . mapM f

both :: (a -> b) -> (a,a) -> (b,b)
both f (a1,a2) = (f a1,f a2)

bothM :: Applicative f => (a -> f b) -> (a,a) -> f (b,b)
bothM f = sequenceTuple . both f

first :: (a -> c) -> (a,b) -> (c,b)
first f (a,b) = (f a,b)

second :: (b -> c) -> (a,b) -> (a,c)
second f (a,b) = (a,f b)

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

listCCFiles :: FilePath -> IO [FilePath]
listCCFiles dir =
    liftM (map (\f -> joinPath [dir,f]) . sort . filter ((==".cc") . takeExtension)) $ listDirectory dir


welcome :: IO ()
welcome = do putStrLn $ "This is the test program for Programming Languages Lab 2"


runMake :: FilePath -> IO ()
runMake dir = do checkDirectoryExists dir
                 runPrgNoFail_ "make" ["-C"] dir

type TestSuite = ([FilePath],[FilePath])

runTests :: FilePath -> TestSuite -> IO ([Bool],[Bool])
runTests dir (goodProgs,badProgs) =
    do let prog = joinPath [dir,executable_name]
       checkFileExists prog
       good <- mapM (testBackendProg prog) goodProgs
       bad  <- mapM (testBadProgram prog) badProgs
       return (good,bad)

testBackendProg :: FilePath -> FilePath -> IO Bool
testBackendProg prog f =
    do input  <- readFileIfExists (f++".input")
       output <- readFileIfExists (f++".output")
       putStr $ "Running " ++ f ++ "... "
       (s,out,err) <- readProcessWithExitCode prog [f] input
       putStrLnExitCode s "."
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
  putStr $ "Running " ++ f ++ "... "
  (s,out,err) <- readProcessWithExitCode prog [f] input
  putStrLnExitCode s "."
  debug $ "Exit code: " ++ show s
  if "TYPE ERROR" `isPrefixOf` out then return True else
    if "SYNTAX ERROR" `isPrefixOf` out then return True else do
      reportError prog "Bad program passed type checking" f "" out err
      return False

--
-- * Main
--

main :: IO ()
main = setup >> getArgs >>= parseArgs >>= uncurry mainOpts

-- | In various contexts this is guessed incorrectly
setup :: IO ()
setup = hSetBuffering stdout LineBuffering

data Options = Options { debugFlag       :: Bool
                       , makeFlag        :: Bool
                       , testSuiteOption :: Maybe TestSuite }

enableDebug :: Options -> Options
enableDebug options = options { debugFlag = True }

disableMake :: Options -> Options
disableMake options = options { makeFlag = False }

addGood, addBad :: FilePath -> Options -> Options
addGood f options = options { testSuiteOption = Just $ maybe ([f],[]) (first  (f:)) $ testSuiteOption options }
addBad  f options = options { testSuiteOption = Just $ maybe ([],[f]) (second (f:)) $ testSuiteOption options }

optDescr :: [OptDescr (Options -> Options)]
optDescr = [ Option []    ["debug"]   (NoArg  enableDebug       ) "print debug messages"
           , Option []    ["no-make"] (NoArg  disableMake       ) "do not run make"
           , Option ['g'] ["good"]    (ReqArg addGood     "FILE") "good test case FILE"
           , Option ['b'] ["bad"]     (ReqArg addBad      "FILE") "bad test case FILE"
           ]

-- | Filter out and process options, return the argument.
parseArgs :: [String] -> IO (FilePath,TestSuite)
parseArgs argv = case getOpt RequireOrder optDescr argv of
  (o,[cfFile],[]) -> do
    let defaultOptions = Options False True Nothing
        options = foldr ($) defaultOptions o
    when (debugFlag options)      $ writeIORef doDebug True
    when (not $ makeFlag options) $ writeIORef doMake  False
    let testSuite    = fromMaybe (["good"],["bad"]) $ testSuiteOption options
        expandPath f = doesDirectoryExist f >>= \b -> if b then listCCFiles f else return [f]
    testSuite' <- bothM (concatMapM expandPath) testSuite
    return (cfFile,testSuite')
  (_,_,_) -> do
    usage
    exitFailure

usage :: IO ()
usage = do
  hPutStrLn stderr "Usage: progs-test-lab2 [--debug] [--no-make] [-g|--good FILE]... [-b|--bad FILE]... interpreter_code_directory"
  exitFailure

mainOpts :: FilePath -> TestSuite -> IO ()
mainOpts dir testSuite =
    do welcome
       domake <- readIORef doMake
       when domake $ runMake dir
       (good,bad) <- runTests dir testSuite
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
#if defined(mingw32_HOST_OS)
color _ s = s
#else
color c s = fgcol c ++ s ++ normal
#endif

highlight, bold, underline, normal :: String
highlight = "\ESC[7m"
bold      = "\ESC[1m"
underline = "\ESC[4m"
normal    = "\ESC[0m"

fgcol, bgcol :: Color -> String
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
  hPutStr stderr $ "Running " ++ c ++ "... "
  (s,out,err) <- readProcessWithExitCode exe (flags ++ [file]) ""
  hPutStrLnExitCode s stderr "."
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

colorExitCode :: ExitCode -> String -> String
colorExitCode ExitSuccess     = color green
colorExitCode (ExitFailure _) = color red

putStrLnExitCode :: ExitCode -> String -> IO ()
putStrLnExitCode e = putStrLn . colorExitCode e

hPutStrLnExitCode :: ExitCode -> Handle -> String -> IO ()
hPutStrLnExitCode e h = hPutStrLn h . colorExitCode e

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
