{-# OPTIONS_GHC -cpp #-}

-- GHC needs -threaded

import Control.Concurrent
import Control.Concurrent.Chan
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

{-# NOINLINE doDebug #-}
doDebug :: IORef Bool
doDebug = unsafePerformIO $ newIORef False

-- | Print debug message if debug is set to @True@.
debug :: String -> IO ()
debug s = do
  d <- readIORef doDebug
  when d $ putStrLn s


listGoodProgs = listCCFiles $ joinPath ["..","good"]

listBadProgs = listCCFiles $ joinPath ["..","bad"]

listCCFiles dir =
  map (\ f -> joinPath [dir,f]) .
  sort .
  filter (("cc" ==) . getExt) <$> getDirectoryContents dir


welcome :: IO ()
welcome = do
  putStrLn $ "This is the test program for Programming Languages Lab 1"
  putStrLn $ color red  $ "NOTE: The start category in your grammar must be called 'Program'"

runBNFC :: IO Int
runBNFC = do
  (out,err) <- runCommandNoFail "bnfc" "Cpp.cf"
  let r = case grep "rules accepted" out of
            []   -> 0
            l:_  -> read $ takeWhile isDigit l
  return r

runAlex :: IO ()
runAlex = runCommandNoFail_ "alex" "LexCpp.x"

runHappy :: IO [String]
runHappy = do
  (out,err) <- runCommandNoFail "happy -i" "ParCpp.y"
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
compileDriver = runCommandNoFail_ ("ghc --make " ++ HC_OPTS ++ " -o testdriver") "testdriver.hs"
#else
compileDriver = runCommandNoFail_ "ghc --make -o testdriver" "testdriver.hs"
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
      c   = "." ++ pathSep : "testdriver " ++ f   -- WINDOWS
  putStrLn $ "Parsing " ++ f ++ "..."
  (out,err,s) <- runCommandStrWait c ""
  case lines err of
    "OK":_    | good -> do return True
              | bad  -> do reportError c "passed BAD program" f "" out err
                           return False
    "ERROR":_ | good -> do reportError c "" f "" out err
                           return False
              | bad  -> do --reportErrorColor green c "failed bad program" f "" out err
                           return True
    _                -> do reportError c "invalid output" f "" out err
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
main = getArgs >>= parseArgs >>= mainOpts

--
-- * List utilities
--

grep :: String -> String -> [String]
grep x = filter (x `isSubStringOf`) . lines
  where isSubStringOf x = any (x `isPrefixOf`) . tails

--
-- * Path name utilities
--

getExt :: FilePath -> String
getExt = reverse . takeWhile (/='.') . reverse

stripExt :: FilePath -> String
stripExt p = if '.' `elem` p then p' else p
  where p' = reverse $ drop 1 $ dropWhile (/='.') $ reverse p

basename :: FilePath -> FilePath
basename = reverse . takeWhile (not . isPathSep) . reverse

isPathSep :: Char -> Bool
isPathSep c = c == pathSep

joinPath :: [String] -> FilePath
joinPath = concat . intersperse [pathSep]

pathSep :: Char
#if defined(mingw32_HOST_OS)
pathSep = '\\'
#else
pathSep = '/'
#endif

--
-- * Either utilities
--

isLeft :: Either a b -> Bool
isLeft = either (const True) (const False)

fromLeft :: Either a b -> a
fromLeft =  either id (error "fromLeft: Right")

catLefts :: [Either a b] -> [a]
catLefts xs = [x | Left x <- xs]

--
-- * Terminal output colors
--

type Color = Int

color :: Color -> String -> String
#if defined(mingw32_HOST_OS)
color c s = s
#else
color c s = fgcol c ++ s ++ normal
#endif

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
blue  = 4

--
-- * Simple pipes
--

type Pipe = Chan (Either Char ())

pipeGetContents :: Pipe -> IO String
pipeGetContents p = do
  s <- getChanContents p
  return $ map fromLeft $ takeWhile isLeft s

pipeWrite :: Pipe -> String -> IO ()
pipeWrite p s = writeList2Chan p (map Left s)

-- close the pipe for writing
pipeClose :: Pipe -> IO ()
pipeClose p = writeChan p (Right ())


--
-- * Various versions of runCommand
--

runCommandChan
  :: String -- ^ command
  -> IO (Pipe,Pipe,Pipe,ProcessHandle) -- ^ stdin, stdout, stderr, process
runCommandChan c = do
 inC  <- newChan
 outC <- newChan
 errC <- newChan
 (pin,pout,perr,p) <- runInteractiveCommand c
 forkIO (pipeGetContents inC >>= hPutStr pin >> hClose pin)
 forkIO (hGetContents pout >>= pipeWrite outC >> pipeClose outC)
 forkIO (hGetContents perr >>= pipeWrite errC >> pipeClose errC)
 return (inC,outC,errC,p)

runCommandStr
  :: String -- ^ command
  -> String -- ^ stdin data
  -> IO (String,String,ProcessHandle) -- ^ stdout, stderr, process
runCommandStr c inStr = do
  (inC,outC,errC,p) <- runCommandChan c
  forkIO (pipeWrite inC inStr >> pipeClose inC)
  out <- pipeGetContents outC
  err <- pipeGetContents errC
  return (out,err,p)

runCommandStrWait
  :: String -- ^ command
  -> String -- ^ stdin data
  -> IO (String,String,ExitCode) -- ^ stdout, stderr, process exit status
runCommandStrWait c inStr = do
  debug $ "Running " ++ show c
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
      lineNo i = let s = show i in replicate (n - length s) ' ' ++ s
      s' = unlines $ zipWith (\ i l -> lineNo i ++ ": " ++ l) [1..] ls
  putStrLn $ color green s'
  putStrLn $ "----------------- end " ++ f ++ " -------------------"


-- | Report how many tests passed.
report :: String -> [Bool] -> IO ()
report n rs = do
  let (p,t) = (length (filter id rs), length rs)
      c = if p == t then green else red
  putStrLn $ color c $
    n ++ "passed " ++ show p ++ " of " ++ show t ++ " tests"
