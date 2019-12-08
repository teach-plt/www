{-# LANGUAGE CPP #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TupleSections #-}

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

concatMapM :: Monad m => (a -> m [b]) -> [a] -> m [b]
concatMapM f = fmap concat . mapM f

tripleM :: Applicative m => (a -> m b) -> (a,a,a) -> m (b,b,b)
tripleM f (x,y,z) = liftA3 (,,) (f x) (f y) (f z)

first3 :: (a -> d) -> (a,b,c) -> (d,b,c)
first3 f (a,b,c) = (f a,b,c)

second3 :: (b -> d) -> (a,b,c) -> (a,d,c)
second3 f (a,b,c) = (a,f b,c)

third3 :: (c -> d) -> (a,b,c) -> (a,b,d)
third3 f (a,b,c) = (a,b,f c)

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

{-# NOINLINE doDebug #-}
doDebug :: IORef Bool
doDebug = unsafePerformIO $ newIORef False

-- | Whether to run make
{-# NOINLINE doMake  #-}
doMake :: IORef Bool
doMake = unsafePerformIO $ newIORef True

-- | Whether to compare actual with expected output
{-# NOINLINE doCmp  #-}
doCmp :: IORef Bool
doCmp = unsafePerformIO $ newIORef True

debug :: String -> IO ()
debug s = do
  d <- readIORef doDebug
  when d $ putStrLn s

listCCFiles :: FilePath -> IO [FilePath]
listCCFiles dir = sort . filter ((==".cc") . takeExtension) <$> listDirectoryRecursive dir

listDirectoryRecursive :: FilePath -> IO [FilePath]
listDirectoryRecursive dir = do
  doesDirectoryExist dir >>= \case
    False -> return []
    True  -> do
      fs <- map (dir </>) <$> listDirectory dir
      (fs ++) <$> concatMapM listDirectoryRecursive fs

welcome :: IO ()
welcome = do putStrLn $ "This is the test program for Programming Languages Lab 2"

runMake :: FilePath -> IO ()
runMake dir = do checkDirectoryExists dir
                 runPrgNoFail_ "make" ["-C"] dir

type TestSuite = ([FilePath],[FilePath],[FilePath])

runTests :: FilePath -> TestSuite -> IO ([(FilePath,Bool)],[(FilePath,Bool)],[(FilePath,Bool)])
runTests dir (goodProgs,badProgs,badRuntimeProgs) = do
  let prog = joinPath [dir,executable_name]
  checkFileExists prog
  good       <- mapM (\f -> (f,) <$> testGoodProgram       prog f) goodProgs
  bad        <- mapM (\f -> (f,) <$> testBadProgram        prog f) badProgs
  badRuntime <- mapM (\f -> (f,) <$> testBadRuntimeProgram prog f) badRuntimeProgs
  return (good,bad,badRuntime)

testGoodProgram :: FilePath -> FilePath -> IO Bool
testGoodProgram prog f = do
  input <- readFileIfExists (f++".input")
  output <- readFileIfExists (f++".output")
  putStr $ "Running " ++ f ++ "... "
  (s,out,err) <- readProcessWithExitCode prog [f] input
  putStrLnExitCode s "."
  debug $ "Exit code: " ++ show s
  -- Try to work around line ending problem
  let removeCR = filter (/= '\r')
  if (trim (removeCR err) /= "")
  then reportError prog "unexpected output on stderr" (Just f) (Just input) (Just out) (nullMaybe err) >>
       return False
  else do docmp <- readIORef doCmp
          if docmp
          then if trim (removeCR out) == trim (removeCR output)
               then return True
               else do reportError prog "invalid output" (Just f) (Just input) (Just out) (nullMaybe err)
                       putStrLn "Expected output:"
                       putStrLn $ color blue $ output
                       return False
          else return True

testBadProgram :: FilePath -> FilePath -> IO Bool
testBadProgram prog f = do
  input <- readFileIfExists (f++".input")
  putStr $ "Running " ++ f ++ "... "
  (s,out,err) <- readProcessWithExitCode prog [f] input
  putStrLnExitCode s "."
  debug $ "Exit code: " ++ show s
  if "TYPE ERROR" `isPrefixOf` out then
    return True
  else do
    reportError prog "Ill-typed program passed type checking" (Just f) Nothing (nullMaybe out) (nullMaybe err)
    return False

testBadRuntimeProgram :: FilePath -> FilePath -> IO Bool
testBadRuntimeProgram prog f = do
  input <- readFileIfExists (f++".input")
  putStr $ "Running " ++ f ++ "... "
  (s,out,err) <- readProcessWithExitCode prog [f] input
  putStrLnExitCode s "."
  debug $ "Exit code: " ++ show s
  docmp <- readIORef doCmp
  if docmp
  then if "INTERPRETER ERROR" `isPrefixOf` out then
         return True
       else do
         reportError prog "Bad (type-correct) program ran to completion without error" (Just f) Nothing (nullMaybe out) (nullMaybe err)
         return False
  else return True

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
                       , cmpFlag         :: Bool
                       , testSuiteOption :: Maybe TestSuite }

enableDebug :: Options -> Options
enableDebug options = options { debugFlag = True }

disableMake :: Options -> Options
disableMake options = options { makeFlag = False }

disableCmp :: Options -> Options
disableCmp options = options { cmpFlag = False }

addGood, addBad :: FilePath -> Options -> Options
addGood       f options = options { testSuiteOption = Just $ maybe ([f],[],[]) (first3  (f:)) $ testSuiteOption options }
addBad        f options = options { testSuiteOption = Just $ maybe ([],[f],[]) (second3 (f:)) $ testSuiteOption options }
addBadRuntime f options = options { testSuiteOption = Just $ maybe ([],[],[f]) (third3  (f:)) $ testSuiteOption options }

optDescr :: [OptDescr (Options -> Options)]
optDescr = [ Option []    ["debug"]       (NoArg  enableDebug         ) "print debug messages"
           , Option []    ["no-make"]     (NoArg  disableMake         ) "do not run make"
           , Option []    ["no-cmp"]      (NoArg  disableCmp          ) "do not compare actual with expected output"
           , Option ['g'] ["good"]        (ReqArg addGood       "FILE") "good test case FILE"
           , Option ['b'] ["bad"]         (ReqArg addBad        "FILE") "bad test case FILE"
           , Option ['r'] ["bad-runtime"] (ReqArg addBadRuntime "FILE") "bad-runtime test case FILE"
           ]

-- | Filter out and process options, return the argument.
parseArgs :: [String] -> IO (FilePath,TestSuite)
parseArgs argv = case getOpt RequireOrder optDescr argv of
  (o,[cfFile],[]) -> do
    let defaultOptions = Options False True True Nothing
        options = foldr ($) defaultOptions o
    when (debugFlag options)      $ writeIORef doDebug True
    when (not $ makeFlag options) $ writeIORef doMake  False
    when (not $ cmpFlag  options) $ writeIORef doCmp   False
    let testSuite    = fromMaybe (["good"],["bad"],["bad-runtime"]) $ testSuiteOption options
        expandPath f = doesDirectoryExist f >>= \b -> if b then listCCFiles f else return [f]
    testSuite' <- tripleM (concatMapM expandPath) testSuite
    return (cfFile,testSuite')
  (_,_,_) -> do
    usage
    exitFailure

usage :: IO ()
usage = do
  hPutStrLn stderr "Usage: progs-test-lab2 [--debug] [--no-make] [--no-cmp]"
  hPutStrLn stderr "           [-g|--good FILE]... [-b|--bad FILE]... [-r|--bad-runtime FILE]..."
  hPutStrLn stderr "           interpreter_code_directory"
  exitFailure

mainOpts :: FilePath -> TestSuite -> IO ()
mainOpts dir testSuite =
    do welcome
       domake <- readIORef doMake
       when domake $ runMake dir
       (good,bad,badRuntime) <- runTests dir testSuite
       putStrLn ""
       putStrLn "------------------------------------------------------------"
       report "Good programs: " good
       report "Bad programs: " bad
       report "Bad runtime programs: " badRuntime

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
           when e $ do putStrLn $ "For input file " ++ f ++ ":"
                       putStrLn $ "---------------- begin " ++ f ++ " ------------------"
                       s <- readFile f
                       putStrLn $ color green s
                       putStrLn $ "----------------- end " ++ f ++ " -------------------"


-- | Report how many tests passed and which tests failed (if any).
report :: String -> [(FilePath,Bool)] -> IO ()
report n rs = do
  let (passedTests,failedTests) = partition snd rs
      (p,t) = (length passedTests, length rs)
      successful = p == t
      c = if successful then green else red
  putStrLn $ color c $
           n ++ "passed " ++ show p ++ " of " ++ show t ++ " tests"
  when (not successful) $ do
    putStrLn $ show (t - p) ++ " tests failed:"
    forM_ failedTests $ \(fp,_) -> putStrLn $ "- " ++ fp
