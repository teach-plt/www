{-# LANGUAGE CPP #-}
{-# LANGUAGE LambdaCase, TupleSections #-}

-- GHC needs -threaded

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
executable_name = "lab3" <.> exeExtension

concatMapM :: Monad m => (a -> m [b]) -> [a] -> m [b]
concatMapM f = fmap concat . mapM f

whenM :: Monad m => m Bool -> m () -> m ()
whenM mb m = mb >>= \b -> when b m

whenJust :: Applicative m => Maybe a -> (a -> m ()) -> m ()
whenJust (Just a) k = k a
whenJust Nothing  _ = pure ()

list :: [a] -> b -> ([a] -> b) -> b
list [] b _ = b
list as _ f = f as

fromList :: [a] -> [a] -> [a]
fromList as xs = list as xs id

nullMaybe :: [a] -> Maybe [a]
nullMaybe as = list as Nothing Just

--
-- * Main
--

main :: IO ()
main = setup >> getArgs >>= parseArgs >>= uncurry mainOpts

-- | In various contexts this is guessed incorrectly
setup :: IO ()
setup = hSetBuffering stdout LineBuffering

data Options = Options { debugFlag       :: Bool
                       , doublesFlag     :: Bool
                       , makeFlag        :: Bool
                       , testSuiteOption :: Maybe TestSuite }

enableDebug :: Options -> Options
enableDebug options = options { debugFlag = True }

disableDoubles :: Options -> Options
disableDoubles options = options { doublesFlag = False }

disableMake :: Options -> Options
disableMake options = options { makeFlag = False }

addTest :: FilePath -> Options -> Options
addTest f options = options { testSuiteOption = Just $ maybe [f] (f:) $ testSuiteOption options }

optDescr :: [OptDescr (Options -> Options)]
optDescr = [ Option []    ["debug"]      (NoArg  enableDebug       ) "print debug messages"
           , Option []    ["no-doubles"] (NoArg  disableDoubles    ) "exclude double tests"
           , Option []    ["no-make"]    (NoArg  disableMake       ) "do not run make"
           , Option ['t'] ["test"]       (ReqArg addTest     "FILE") "good test case FILE"
           ]

-- | Filter out and process options, return the argument and the rest.
parseArgs :: [String] -> IO (FilePath,TestSuite)
parseArgs argv = case getOpt RequireOrder optDescr argv of
  (o,[progdir],[]) -> do
    let defaultOptions = Options False True True Nothing
        options = foldr ($) defaultOptions o
    when (debugFlag   options)       $ writeIORef doDebug            True
    when (not $ doublesFlag options) $ writeIORef includeDoubleTests False
    when (not $ makeFlag options)    $ writeIORef doMake             False
    let testSuite    = fromMaybe ["good", "dir-for-path-test/one-more-dir"] $ testSuiteOption options
        expandPath f = doesDirectoryExist f >>= \b -> if b then listCCFiles f else return [f]
    testSuite' <- concatMapM expandPath testSuite
    return (progdir,testSuite')
  (_,_,_) -> do
    usage
    exitFailure

usage :: IO ()
usage = do
  hPutStrLn stderr "Usage: progs-test-lab3 [--debug] [--no-doubles] [--no-make] [-t|--test FILE]..."
  hPutStrLn stderr "           compiler_code_directory"
  exitFailure

mainOpts :: FilePath -> TestSuite -> IO ()
mainOpts progdir testSuite = do
  putStrLn "This is the test program for Programming Languages Lab 3"
  -- Cleanup files from old runs
  forM_ testSuite (\f -> cleanFiles $ map (replaceExtension f) [".j", ".class"])
  domake <- readIORef doMake
  when domake $ runMake progdir
  good <- runTests progdir testSuite
  putStrLn ""
  putStrLn "------------------------------------------------------------"
  report "Good programs: " good

--
-- * Test driver
--

-- | Whether to run tests involving doubles
{-# NOINLINE includeDoubleTests  #-}
includeDoubleTests :: IORef Bool
includeDoubleTests = unsafePerformIO $ newIORef True

-- | Whether to run make
{-# NOINLINE doMake  #-}
doMake :: IORef Bool
doMake = unsafePerformIO $ newIORef True

-- | Run "make" in given directory.
runMake :: FilePath -> IO ()
runMake dir = do
  checkDirectoryExists dir
  runPrgNoFail_ "make" ["-C"] dir

type TestSuite = [FilePath]

-- | Run test on all ".cc" files in given directories (default "good").
runTests :: FilePath -> TestSuite -> IO [(FilePath,Bool)]
runTests dir files = do
  let prog = joinPath [dir, executable_name]
  checkFileExists prog
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
  putStr $ "Running " ++ f ++ "... "
  (compilerRet, compilerOut, compilerErr) <- readProcessWithExitCode prog [f] ""
  putStrLnExitCode compilerRet "."
  if isExitFailure compilerRet then do
    reportError prog "non-zero exit code" (Just f) (nullMaybe input) (nullMaybe compilerOut) (nullMaybe compilerErr)
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
        reportError "java" "non-zero exit code" (Just f) (nullMaybe input) (nullMaybe javaOut) (nullMaybe javaErr)
        return False
      else do
        -- Try to work around line ending problem
        let removeCR = filter (/= '\r')
        if trim (removeCR javaOut) == trim (removeCR output) then
          return True
        else do
          reportError "java" "invalid output" (Just f) (nullMaybe input) (nullMaybe javaOut) (nullMaybe javaErr)
          putStrLn "Expected output:"
          putStrLn $ color blue $ output
          return False
    else do
      reportError prog ("did not find any Java class file at \"" ++ expectedJavaClassFilePath ++ "\" (note that the output Java class file must be written to same directory as the input C++ file)") (Just f) (nullMaybe input) (nullMaybe compilerOut) (nullMaybe compilerErr)
      return False

listCCFiles :: FilePath -> IO [FilePath]
listCCFiles dir = do
  doubles <- readIORef includeDoubleTests
  sort . filter (doublesFilter doubles) . filter ((==".cc") . takeExtension) <$> listDirectoryRecursive dir
  where doublesFilter doubles filename = doubles || not (isInfixOf "double" filename || isInfixOf "subtyping" filename)

listDirectoryRecursive :: FilePath -> IO [FilePath]
listDirectoryRecursive dir = do
  doesDirectoryExist dir >>= \case
    False -> return []
    True  -> do
      fs <- map (dir </>) <$> listDirectory dir
      (fs ++) <$> concatMapM listDirectoryRecursive fs

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

trim :: String -> String
trim = f . f
  where f = reverse . dropWhile isSpace

cleanDirectory :: FilePath -> [String] -> IO ()
cleanDirectory path exts = listDirectory path >>=
                           mapM_ (\f -> do let pathf = path </> f
                                           when (takeExtension f `elem` exts) $ cleanFile pathf)

cleanFile :: FilePath -> IO ()
cleanFile file = whenM (doesFileExist file) $ removeFile file

cleanFiles :: [FilePath] -> IO ()
cleanFiles = mapM_ cleanFile

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
  hPutStr stderr $ "Running " ++ c ++ "... "
  (s,out,err) <- readProcessWithExitCode exe (flags ++ [file]) ""
  hPutStrLnExitCode s stderr "."
  case s of
    ExitFailure x -> do
      reportError exe ("with status " ++ show x) (Just file) Nothing (nullMaybe out) (nullMaybe err)
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

colorExitCode :: ExitCode -> String -> String
colorExitCode ExitSuccess     = color green
colorExitCode (ExitFailure _) = color red

putStrLnExitCode :: ExitCode -> String -> IO ()
putStrLnExitCode e = putStrLn . colorExitCode e

hPutStrLnExitCode :: ExitCode -> Handle -> String -> IO ()
hPutStrLnExitCode e h = hPutStrLn h . colorExitCode e

reportErrorColor :: Color
                 -> String         -- ^ command that failed
                 -> String         -- ^ how it failed
                 -> Maybe FilePath -- ^ source file
                 -> Maybe String   -- ^ given input
                 -> Maybe String   -- ^ stdout output
                 -> Maybe String   -- ^ stderr output
                 -> IO ()
reportErrorColor col c m f i o e =
    do
    putStrLn $ color col $ c ++ " failed: " ++ m
    whenJust f prFile
    whenJust i $ \i -> do
                       putStrLn "Given this input:"
                       putStrLn $ color blue $ fromList i "<nothing>"
    whenJust o $ \o -> do
                       putStrLn "It printed this to standard output:"
                       putStrLn $ color blue $ fromList o "<nothing>"
    whenJust e $ \e -> do
                       putStrLn "It printed this to standard error:"
                       putStrLn $ color blue $ fromList e "<nothing>"

reportError :: String         -- ^ command that failed
            -> String         -- ^ how it failed
            -> Maybe FilePath -- ^ source file
            -> Maybe String   -- ^ given input
            -> Maybe String   -- ^ stdout output
            -> Maybe String   -- ^ stderr output
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
  let (passed, failed) = partition snd rs
  let (p,t) = (length passed, length rs)
      c     = if p == t then green else red
  putStrLn $ color c $ n ++ "passed " ++ show p ++ " of " ++ show t ++ " tests"
  unless (null failed) $
    mapM_ (putStrLn . color red) $ "Failed tests:" : map fst failed
