{-# LANGUAGE CPP #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TupleSections #-}

{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

-- GHC needs -threaded

import Control.Applicative
import Control.Exception
import Control.Monad

import Data.Char
import Data.IORef
import Data.List (isInfixOf, partition, sort)
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
first3 f (a, b, c) = (f a, b, c)

second3 :: (b -> d) -> (a,b,c) -> (a,d,c)
second3 f (a, b, c) = (a, f b, c)

third3 :: (c -> d) -> (a,b,c) -> (a,b,d)
third3 f (a, b, c) = (a, b, f c)

whenJust :: Applicative m => Maybe a -> (a -> m ()) -> m ()
whenJust (Just a) k = k a
whenJust Nothing  _ = pure ()

ifNull :: [a] -> b -> ([a] -> b) -> b
ifNull [] b _ = b
ifNull as _ f = f as

-- | @replaceNull xs def@ returns @def@ if @xs@ is 'null' and @xs@ otherwise.
replaceNull :: [a] -> [a] -> [a]
replaceNull as xs = ifNull as xs id

-- | 'Nothing' if list is 'null', otherwise 'Just'.
nullMaybe :: [a] -> Maybe [a]
nullMaybe as = ifNull as Nothing Just

{-# NOINLINE doDebug #-}
doDebug :: IORef Bool
doDebug = unsafePerformIO $ newIORef False

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
welcome = putStrLn $ "This is the test program for Programming Languages Lab 2"

-- | Try to build the solution, first with @make@ and then with @cabal@.
runMake :: Bool -> FilePath -> IO ()
runMake tryCabal dir = do
  checkDirectoryExists dir
  withCurrentDirectory dir $ do

    -- Check if there is a cabal file.
    haveCabal <- if tryCabal then doesFileExist "lab2.cabal" else pure False

    -- Run "make" first.
    hPutStrLn stderr $ unwords [ dir, "$", "make" ]
    (exit, out, err) <- readProcessWithExitCode "make" [] ""
    case exit of

      -- "make" succeeded, all is well.
      ExitSuccess -> do
        hPutStrLn stderr $ "Running make succeeded"
        debug $ "Standard output:\n" ++ out
        debug $ "Standard error:\n" ++ err

      -- "make" failed.
      ExitFailure makeExitCode

        -- If we do not have a .cabal file, we have to give up.
        | not haveCabal -> do
            makeErr
            exitFailure

        -- Otherwise, try building with "cabal".
        | otherwise     -> do
            let cmd = "cabal install --installdir=."
            hPutStrLn stderr $ unwords [ dir, "$", cmd ]
            (exit2, out2, err2) <- readProcessWithExitCode "cabal" ["install", "--installdir=."] ""
            case exit2 of

              -- "cabal" succeeded, all is well.
              ExitSuccess -> do
                hPutStrLn stderr $ unwords ["Running", cmd, "after make succeeded"]
                debug $ "Standard output (make):\n" ++ out
                debug $ "Standard error  (make):\n" ++ err
                debug $ "Standard output (cabal):\n" ++ out2
                debug $ "Standard error  (cabal):\n" ++ err2

              -- "cabal" failed; we exhausted our options.
              ExitFailure cabalExitCode -> do
                -- Both failed, so we crash.
                makeErr
                cabalErr
                exitFailure
                where
                cabalErr = reportError cmd ("with status " ++ show cabalExitCode) (Just dir) Nothing (nullMaybe out2) (nullMaybe err2)

        where
        makeErr  = reportError "make" ("with status " ++ show makeExitCode) (Just dir) Nothing (nullMaybe out) (nullMaybe err)


type TestSuite = ([FilePath],[FilePath],[FilePath])

runTests :: FilePath -> TestSuite -> IO ([(FilePath,Bool)],[(FilePath,Bool)],[(FilePath,Bool)])
runTests dir (goodProgs, badProgs, badRuntimeProgs) = do
  let prog = dir </> executable_name
  checkFileExists prog
  good       <- mapM (\f -> (f,) <$> testGoodProgram       prog f) goodProgs
  bad        <- mapM (\f -> (f,) <$> testBadProgram        prog f) badProgs
  badRuntime <- mapM (\f -> (f,) <$> testBadRuntimeProgram prog f) badRuntimeProgs
  return (good, bad, badRuntime)

testGoodProgram :: FilePath -> FilePath -> IO Bool
testGoodProgram prog f = do
  input  <- readFileIfExists $ f ++ ".input"
  output <- readFileIfExists $ f ++ ".output"
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
  input <- readFileIfExists $ f ++ ".input"
  putStr $ "Running " ++ f ++ "... "
  (s,out,err) <- readProcessWithExitCode prog [f] input
  putStrLnExitCode s "."
  debug $ "Exit code: " ++ show s
  -- A. Abel, 2020-11-18 more lenient checking for error report.
  -- Can be in either stdout or stderr, and need not be the first thing that is printed.
  if "TYPE ERROR" `isInfixOf` (out ++ err) then
    return True
  else do
    let msg = case s of
          ExitSuccess{} -> "Ill-typed program passed type checking"
          ExitFailure{} -> "Expected output TYPE ERROR, but this was not printed"
    reportError prog msg (Just f) Nothing (nullMaybe out) (nullMaybe err)
    return False

testBadRuntimeProgram :: FilePath -> FilePath -> IO Bool
testBadRuntimeProgram prog f = do
  input <- readFileIfExists $ f++ ".input"
  putStr $ "Running " ++ f ++ "... "
  (s,out,err) <- readProcessWithExitCode prog [f] input
  putStrLnExitCode s "."
  debug $ "Exit code: " ++ show s
  docmp <- readIORef doCmp
  if not docmp then
    return True
  -- A. Abel, 2020-11-18 more lenient checking for error report.
  -- Can be in either stdout or stderr, and need not be the first thing that is printed.
  else if "INTERPRETER ERROR" `isInfixOf` (out ++ err) then
    return True
  else do
    let msg = case s of
          ExitSuccess{} -> "Bad (but type-correct) program ran to completion without error"
          ExitFailure{} -> "Expected output INTERPRETER ERROR, but this was not printed"
    reportError prog msg (Just f) Nothing (nullMaybe out) (nullMaybe err)
    return False


--
-- * Main
--

main :: IO ()
main = do
  setup
  (options, dir, testSuite) <- parseArgs =<< getArgs
  mainOpts options dir testSuite

-- | In various contexts this is guessed incorrectly
setup :: IO ()
setup = hSetBuffering stdout LineBuffering

data Options = Options
  { debugFlag       :: Bool
  , cabalFlag       :: Bool
      -- ^ Try building with @cabal@ if @make@ failed?
  , makeFlag        :: Bool
  , cmpFlag         :: Bool
  , testSuiteOption :: Maybe TestSuite
  }

enableDebug :: Options -> Options
enableDebug options = options { debugFlag = True }

enableCabal :: Options -> Options
enableCabal options = options { cabalFlag = True }

disableMake :: Options -> Options
disableMake options = options { makeFlag = False }

disableCmp :: Options -> Options
disableCmp options = options { cmpFlag = False }

addGood, addBad, addBadRuntime :: FilePath -> Options -> Options
addGood       f options = options { testSuiteOption = Just $ maybe ([f],[],[]) (first3  (f:)) $ testSuiteOption options }
addBad        f options = options { testSuiteOption = Just $ maybe ([],[f],[]) (second3 (f:)) $ testSuiteOption options }
addBadRuntime f options = options { testSuiteOption = Just $ maybe ([],[],[f]) (third3  (f:)) $ testSuiteOption options }

optDescr :: [OptDescr (Options -> Options)]
optDescr = [ Option []    ["debug"]       (NoArg  enableDebug         ) "print debug messages"
           , Option []    ["try-cabal"]   (NoArg  enableCabal         ) "try building with cabal after make failed"
           , Option []    ["no-make"]     (NoArg  disableMake         ) "do not run make"
           , Option []    ["no-cmp"]      (NoArg  disableCmp          ) "do not compare actual with expected output"
           , Option ['g'] ["good"]        (ReqArg addGood       "FILE") "good test case FILE"
           , Option ['b'] ["bad"]         (ReqArg addBad        "FILE") "bad test case FILE"
           , Option ['r'] ["bad-runtime"] (ReqArg addBadRuntime "FILE") "bad-runtime test case FILE"
           ]

-- | Filter out and process options, return the argument.
parseArgs :: [String] -> IO (Options, FilePath, TestSuite)
parseArgs argv = case getOpt RequireOrder optDescr argv of
  (o,[cfFile],[]) -> do
    let defaultOptions = Options False False True True Nothing
        options = foldr ($) defaultOptions o
    when (debugFlag options)      $ writeIORef doDebug True
    when (not $ cmpFlag  options) $ writeIORef doCmp   False
    let testSuite    = fromMaybe (["good"],["bad"],["bad-runtime"]) $ testSuiteOption options
        expandPath f = doesDirectoryExist f >>= \b -> if b then listCCFiles f else return [f]
    testSuite' <- tripleM (concatMapM expandPath) testSuite
    return (options, cfFile, testSuite')
  (_,_,_) -> do
    usage
    exitFailure

usage :: IO ()
usage = do
  hPutStrLn stderr "Usage: plt-test-lab2 [--debug] [--no-make] [--no-cmp]"
  hPutStrLn stderr "           [-g|--good FILE]... [-b|--bad FILE]... [-r|--bad-runtime FILE]..."
  hPutStrLn stderr "           interpreter_code_directory"
  exitFailure

mainOpts :: Options -> FilePath -> TestSuite -> IO ()
mainOpts options dir testSuite = do
  welcome
  when (makeFlag options) $ runMake (cabalFlag options) dir
  (good, bad, badRuntime) <- runTests dir testSuite
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
color c s
  | haveColors = fgcol c ++ s ++ normal
  | otherwise  = s
#endif

-- | Colors are disabled if the terminal does not support them.
{-# NOINLINE haveColors #-}
haveColors :: Bool
haveColors = unsafePerformIO supportsPretty

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
                       putStrLn $ color blue $ replaceNull i "<nothing>"
    whenJust o $ \o -> do
                       putStrLn "It printed this to standard output:"
                       putStrLn $ color blue $ replaceNull o "<nothing>"
    whenJust e $ \e -> do
                       putStrLn "It printed this to standard error:"
                       putStrLn $ color blue $ replaceNull e "<nothing>"

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

-- Inlined from https://hackage.haskell.org/package/pretty-terminal-0.1.0.0/docs/src/System-Console-Pretty.html#supportsPretty :

-- | Whether or not the current terminal supports pretty-terminal
supportsPretty :: IO Bool
supportsPretty =
  hSupportsANSI stdout
  where
    -- | Use heuristics to determine whether the functions defined in this
    -- package will work with a given handle.
    --
    -- The current implementation checks that the handle is a terminal, and
    -- that the @TERM@ environment variable doesn't say @dumb@ (whcih is what
    -- Emacs sets for its own terminal).
    hSupportsANSI :: Handle -> IO Bool
    -- Borrowed from an HSpec patch by Simon Hengel
    -- (https://github.com/hspec/hspec/commit/d932f03317e0e2bd08c85b23903fb8616ae642bd)
    hSupportsANSI h = (&&) <$> hIsTerminalDevice h <*> (not <$> isDumb)
      where
        isDumb = (== Just "dumb") <$> lookupEnv "TERM"
