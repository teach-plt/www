{-# LANGUAGE CPP #-}

-- GHC needs -threaded

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

{-# NOINLINE doDebug #-}
doDebug :: IORef Bool
doDebug = unsafePerformIO $ newIORef False

-- | Print debug message if debug is set to @True@.
debug :: String -> IO ()
debug s = do
  d <- readIORef doDebug
  when d $ putStrLn s

listGoodProgs, listBadProgs :: IO [FilePath]
listGoodProgs = listCCFiles $ joinPath ["..","good"]
listBadProgs  = listCCFiles $ joinPath ["..","bad"]

listCCFiles :: FilePath -> IO [FilePath]
listCCFiles dir =
  map (\f -> joinPath [dir,f]) .
  sort .
  filter ((".cc" ==) . takeExtension) <$> listDirectory dir


welcome :: IO ()
welcome = do
  putStrLn $ "This is the test program for Programming Languages Lab 1"
  putStrLn $ color red  $ "NOTE: The start category in your grammar must be called 'Program'"

runBNFC :: IO Int
runBNFC = do
  (out,_) <- runPrgNoFail "bnfc" [] "Cpp.cf"
  let r = case grep "rules accepted" out of
            []   -> 0
            l:_  -> read $ takeWhile isDigit l
  return r

runAlex :: IO ()
runAlex = runPrgNoFail_ "alex" [] "LexCpp.x"

runHappy :: IO [String]
runHappy = do
  (_,err) <- runPrgNoFail "happy" ["-i"] "ParCpp.y"
  return $ grep "conflict" err

writeDriver :: IO ()
writeDriver = writeFile "testdriver.hs" $ unlines
  [ "import System.IO (stderr, hPutStrLn)"
  , "import System.Environment (getArgs)"
  , "import LexCpp;import ParCpp;import ErrM"
  , "parse s = case pProgram (myLexer s) of"
  , "            Bad err  -> hPutStrLn stderr \"ERROR\" >> hPutStrLn stderr err"
  , "            Ok  tree -> hPutStrLn stderr \"OK\""
  , "main = do [file] <- getArgs"
  , "          readFile file >>= parse"
  ]

compileDriver :: IO ()
#ifdef HC_OPTS
compileDriver = runPrgNoFail_ "ghc" (["--make"] ++ words HC_OPTS ++ ["-o", "testdriver"]) "testdriver.hs"
#else
compileDriver = runPrgNoFail_ "ghc" ["--make", "-o", "testdriver"] "testdriver.hs"
#endif

runTests :: IO ([Bool],[Bool])
runTests = do
  goodProgs <- listGoodProgs
  badProgs  <- listBadProgs
  good <- mapM (testFrontendProg True)  goodProgs
  bad  <- mapM (testFrontendProg False) badProgs
  return (good,bad)


testFrontendProg :: Bool -> FilePath -> IO Bool
testFrontendProg good f = do
  let bad = not good
      prg = "." </> "testdriver"
  putStrLn $ "Parsing " ++ f ++ "..."
  -- TODO: Should maybe check exit code here
  (_,out,err) <- readProcessWithExitCode prg [f] ""
  case lines err of
    "OK":_    | good -> return True
              | bad  -> do reportError prg "passed BAD program" f "" out err
                           return False
    "ERROR":_ | good -> do reportError prg "" f "" out err
                           return False
              | bad  -> return True
    _                -> do reportError prg "invalid output" f "" out err
                           return False

--
-- * Main
--

parseArgs :: [String] -> IO String
parseArgs ["-debug", cfFile] = cfFile <$ writeIORef doDebug True
parseArgs [cfFile]           = return cfFile
parseArgs _                  = do
  hPutStrLn stderr "Usage: progs-test-lab1 [-debug] cf_file"
  exitFailure

mainOpts :: FilePath -> IO ()
mainOpts cfFile = do
  welcome
  let dir = "lab1-test-dir"
  createDirectoryIfMissing True dir
  copyFile cfFile $ joinPath [dir,"Cpp.cf"]
  setCurrentDirectory dir
  rules <- runBNFC
  runAlex
  msgs <- runHappy
  writeDriver
  compileDriver
  (good,bad) <- runTests
  putStrLn ""
  putStrLn "------------------------------------------------------------"
  putStrLn $ color (if rules > 150 then red else black) $ "Number of rules:         " ++ show rules
  when (not (null msgs)) $ do
    mapM_ (putStrLn . color blue) msgs
    putStrLn $ "See " ++ joinPath [dir, "ParCpp.info"]
               ++ " for information about the conflicts"
  putStrLn "------------------------------------------------------------"
  report "Good programs: " good
  report "Bad programs:  " bad

main :: IO ()
main = setup >> getArgs >>= parseArgs >>= mainOpts

-- | In various contexts this is guessed incorrectly
setup :: IO ()
setup = hSetBuffering stdout LineBuffering

--
-- * List utilities
--

grep :: String -> String -> [String]
grep x = filter (x `isSubStringOf`) . lines

isSubStringOf :: String -> String -> Bool
isSubStringOf x = any (x `isPrefixOf`) . tails

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
-- * Error reporting and output checking
--

reportErrorColor
  :: Color
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
    putStrLn $ "For source file " ++ f ++ ":"
    prFile f
    when (not (null i)) $ do
      putStrLn "Given this input:"
      putStrLn $ color blue $ i
    when (not (null o)) $ do
      putStrLn "It printed this to standard output:"
      putStrLn $ color blue $ o
    when (not (null e)) $ do
      putStrLn "It printed this to standard error:"
      putStrLn $ color blue $ e

reportError
  :: String -- ^ command that failed
  -> String -- ^ how it failed
  -> FilePath -- ^ source file
  -> String -- ^ given input
  -> String -- ^ stdout output
  -> String -- ^ stderr output
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

-- | Report how many tests passed.
report :: String -> [Bool] -> IO ()
report n rs = do
  let (p,t) = (length (filter id rs), length rs)
      c = if p == t then green else red
  putStrLn $ color c $
    n ++ "passed " ++ show p ++ " of " ++ show t ++ " tests"
