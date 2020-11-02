{-# LANGUAGE CPP #-}
{-# LANGUAGE TupleSections #-}

-- GHC needs -threaded

import Control.Arrow ((***))
import Control.Applicative
import Control.Monad

import Data.Char
import Data.IORef
import qualified Data.List as List
import Data.Maybe

import System.Console.GetOpt
import System.Directory
  ( copyFile
  , createDirectoryIfMissing
  , doesDirectoryExist
  , listDirectory              -- Requires ghc ≥ 7.10.3
  , setCurrentDirectory
  )
import System.Environment
import System.FilePath
import System.Exit
import System.IO
import System.Process
import System.IO.Unsafe

-- | Configure me!
grammar :: String
grammar = "CMM"

{-# NOINLINE doDebug #-}
doDebug :: IORef Bool
doDebug = unsafePerformIO $ newIORef False

-- | Print debug message if debug is set to @True@.
debug :: String -> IO ()
debug s = do
  d <- readIORef doDebug
  when d $ putStrLn s

listCCFiles :: FilePath -> IO [FilePath]
listCCFiles dir =
  map (dir </>) .
  List.sort .
  filter ((".cc" ==) . takeExtension) <$> listDirectory dir


welcome :: IO ()
welcome = do
  putStrLn $ "This is the test program for Programming Languages Lab 1"
  putStrLn $ color red $ "NOTE: The start category in your grammar must be called 'Program'"

-- | Run @bnfc@ and return the number of rules of the grammar.
runBNFC :: IO Int
runBNFC = do
  (out,_) <- runPrgNoFail "bnfc" ["-d"] $ grammar <.> "cf"
  -- Look for line "nnn rules accepted"
  return $ case grep "rules accepted" out of
            []   -> 0
            l:_  -> read $ takeWhile isDigit l

runAlex :: IO ()
runAlex = runPrgNoFail_ "alex" [] $ grammar </> "Lex.x"

runHappy :: IO [String]
runHappy = do
  (_,err) <- runPrgNoFail "happy" ["-i"] $ grammar </> "Par.y"
  return $ grep "conflict" err

writeDriver :: IO ()
writeDriver = writeFile "testdriver.hs" $ unlines
  [ "import System.IO (stderr, hPutStrLn)"
  , "import System.Environment (getArgs)"
  , ""
  , "import " ++ grammar ++ ".Par (myLexer, pProgram)"
  , "import " ++ grammar ++ ".ErrM"
  , ""
  , "parse s = case pProgram (myLexer s) of"
  , "  Bad err  -> hPutStrLn stderr \"ERROR\" >> hPutStrLn stderr err"
  , "  Ok  tree -> hPutStrLn stderr \"OK\""
  , ""
  , "main = do"
  , "  [file] <- getArgs"
  , "  readFile file >>= parse"
  ]

compileDriver :: IO ()
#ifdef HC_OPTS
compileDriver = runPrgNoFail_ "ghc" (["--make"] ++ words HC_OPTS ++ ["-o", "testdriver"]) "testdriver.hs"
#else
compileDriver = runPrgNoFail_ "ghc" ["--make", "-o", "testdriver"] "testdriver.hs"
#endif

type TestSuite = ([FilePath],[FilePath])

runTests :: TestSuite -> IO ([(FilePath,Bool)],[(FilePath,Bool)])
runTests (goodProgs,badProgs) = do
  good <- mapM (\f -> (f,) <$> testFrontendProg True  f) goodProgs
  bad  <- mapM (\f -> (f,) <$> testFrontendProg False f) badProgs
  return (good,bad)


testFrontendProg :: Bool -> FilePath -> IO Bool
testFrontendProg good f = do
  let bad = not good
      -- For some reason this executable name, without .exe, works on Windows;
      -- I think this is because we are in the same directory as the executable
      prg = "." </> "testdriver"
  putStr $ "Parsing " ++ f ++ "... "
  -- TODO: Should maybe check exit code here
  (s, out, err) <- readProcessWithExitCode prg [f] ""
  putStrLnExitCode s "."
  case lines err of
    "OK":_    | good -> return True
              | bad  -> False <$ reportError prg "passed BAD program" (Just f) Nothing out err

    "ERROR":_ | good -> False <$ reportError prg ""                   (Just f) Nothing out err

              | bad  -> return True
    _                -> False <$ reportError prg "invalid output"     (Just f) Nothing out err


--
-- * Main
--

data Options = Options
  { optDebug :: Bool        -- ^ Print debug information?
  , optGood  :: [FilePath]  -- ^ Good tests.
  , optBad   :: [FilePath]  -- ^ Bad tests.
  }

defaultOptions :: Options
defaultOptions = Options
  { optDebug = False
  , optGood  = []
  , optBad   = []
  }

optDescr :: [OptDescr (Options -> Options)]
optDescr =
  [ Option []    ["debug"] (NoArg  enableDebug       ) "print debug messages"
  , Option ['g'] ["good"]  (ReqArg addGood     "FILE") "good test case FILE"
  , Option ['b'] ["bad"]   (ReqArg addBad      "FILE") "bad test case FILE"
  ]

enableDebug :: Options -> Options
enableDebug o = o { optDebug = True }

addGood, addBad :: FilePath -> Options -> Options
addGood f o = o { optGood = f : optGood o }
addBad  f o = o { optBad  = f : optBad  o }

-- | If no test cases are given, assume standard test suites.
testSuiteOption :: Options -> TestSuite
testSuiteOption o = if null g && null b then (["good"], ["bad"]) else (g, b)
  where
  g = optGood o
  b = optBad  o

parseArgs :: [String] -> IO (FilePath,TestSuite)
parseArgs argv = case getOpt RequireOrder optDescr argv of

  (o,[cfFile],[]) -> do
    let options = foldr ($) defaultOptions o
    when (optDebug options) $ writeIORef doDebug True
    let expandPath f = doesDirectoryExist f >>= \b -> if b then listCCFiles f else return [f]
    testSuite' <- bothM ((concat <$>) . mapM expandPath) $ testSuiteOption options
    return (cfFile, testSuite')

  (_,_,_) -> do
    hPutStrLn stderr "Usage: progs-test-lab1 [--debug] [-g|--good FILE]... [-b|--bad FILE]... cf_file"
    exitFailure

  where
  bothM :: Applicative f => (a -> f b) -> (a,a) -> f (b,b)
  bothM f (a1,a2) = (,) <$> f a1 <*> f a2


mainOpts :: FilePath -> TestSuite -> IO ()
mainOpts cfFile testSuite = do
  welcome
  let dir = "lab1-test-dir"
  createDirectoryIfMissing True dir
  copyFile cfFile $ dir </> grammar <.> "cf"
  setCurrentDirectory dir
  let adjustPath f = if isRelative f then ".." </> f else f
      testSuite'   = (map adjustPath *** map adjustPath) testSuite
  rules <- runBNFC
  runAlex
  msgs <- runHappy
  writeDriver
  compileDriver
  (good,bad) <- runTests testSuite'
  putStrLn ""
  putStrLn "------------------------------------------------------------"
  putStrLn $ color (if rules > 150 then red else black) $ "Number of rules:         " ++ show rules
  unless (null msgs) $ do
    mapM_ (putStrLn . color blue) msgs
    putStrLn $ "See " ++ joinPath [dir, grammar, "Par.info"]
               ++ " for information about the conflicts"
  putStrLn "------------------------------------------------------------"
  report "Good programs: " good
  report "Bad programs:  " bad

main :: IO ()
main = setup >> getArgs >>= parseArgs >>= uncurry mainOpts

-- | In various contexts this is guessed incorrectly
setup :: IO ()
setup = hSetBuffering stdout LineBuffering

--
-- * List utilities
--

-- | Return lines of second argument that contain the first argument.
grep :: String -> String -> [String]
grep x = filter (x `List.isInfixOf`) . lines

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
blue  = 4

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
      reportError exe ("with status " ++ show x) (Just file) Nothing out err
      exitFailure
    ExitSuccess -> do
      debug $ "Standard output:\n" ++ out
      debug $ "Standard error:\n" ++ err
      return (out,err)

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

reportErrorColor
  :: Color
  -> String         -- ^ command that failed
  -> String         -- ^ how it failed
  -> Maybe FilePath -- ^ source file
  -> Maybe String   -- ^ given input
  -> String         -- ^ stdout output
  -> String         -- ^ stderr output
  -> IO ()
reportErrorColor col c m f i o e = do
    putStrLn $ color col $ c ++ " failed: " ++ m
    mapM_ prFile f
    forM_ i $ \ i -> do
      putStrLn "Given this input:"
      putStrLn $ color blue $ if null i then "<nothing>" else i
    unless (null o) $ do
      putStrLn "It printed this to standard output:"
      putStrLn $ color blue o
    unless (null e) $ do
      putStrLn "It printed this to standard error:"
      putStrLn $ color blue e

reportError
  :: String         -- ^ command that failed
  -> String         -- ^ how it failed
  -> Maybe FilePath -- ^ source file
  -> Maybe String   -- ^ given input
  -> String         -- ^ stdout output
  -> String         -- ^ stderr output
  -> IO ()
reportError = reportErrorColor red

prFile :: FilePath -> IO ()
prFile f = do
  putStrLn $ "---------------- begin " ++ f ++ " ------------------"
  s <- readFile f
  let ls = lines s
      n  = length $ show $ length ls
      s' = unlines $ zipWith (\ i l -> rightAlign n (show i) ++ ": " ++ l) [(1 :: Integer)..] ls
  putStrLn $ color green s'
  putStrLn $ "----------------- end " ++ f ++ " -------------------"

rightAlign :: Int -> String -> String
rightAlign w s = replicate (w - length s) ' ' ++ s

-- | Report how many tests passed and which tests failed (if any).
report :: String -> [(FilePath,Bool)] -> IO ()
report n rs = do
  let (passedTests,failedTests) = List.partition snd rs
      (p,t) = (length passedTests, length rs)
      successful = p == t
      c = if successful then green else red
  putStrLn $ color c $
           n ++ "passed " ++ show p ++ " of " ++ show t ++ " tests"
  when (not successful) $ do
    putStrLn $ show (t - p) ++ " tests failed:"
    forM_ failedTests $ \(fp,_) -> putStrLn $ "- " ++ fp
