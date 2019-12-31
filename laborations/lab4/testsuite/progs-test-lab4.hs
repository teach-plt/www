{-# LANGUAGE CPP               #-}
{-# LANGUAGE ViewPatterns      #-}
-- Test suite for lab 4
import Control.Applicative hiding (empty)
import Control.Monad

import Data.Char
import Data.Function
import Data.IORef
import Data.List
import Data.Maybe
import Data.Monoid

import System.Console.GetOpt
import System.Directory
import System.Environment
import System.Exit
import System.FilePath
import System.IO
import System.IO.Unsafe
import System.Process

-- Executable name
executable_name :: FilePath
-- You might have to add or remove .exe here if you are using Windows
executable_name = "lab4" <.> exeExtension

sequenceTuple :: Applicative f => (f a,f b) -> f (a,b)
sequenceTuple = uncurry $ liftA2 (,)

concatMapM :: Monad m => (a -> m [b]) -> [a] -> m [b]
concatMapM f = fmap concat . mapM f

mapTuple :: (a -> c) -> (b -> d) -> (a,b) -> (c,d)
mapTuple f g (a,b) = (f a,g b)

mapTupleM :: Applicative f => (a -> f c) -> (b -> f d) -> (a,b) -> f (c,d)
mapTupleM f g = sequenceTuple . mapTuple f g

first :: (a -> c) -> (a,b) -> (c,b)
first f = mapTuple f id

second :: (b -> c) -> (a,b) -> (a,c)
second f = mapTuple id f

first3 :: (a -> d) -> (a,b,c) -> (d,b,c)
first3 f (a,b,c) = (f a,b,c)

splitOn :: Char -> String -> [String]
splitOn _   "" = []
splitOn sep s  = splitOn' s ""
  where
    splitOn' []     sub             = [reverse sub]
    splitOn' (c:cs) sub | c == sep  = reverse sub:splitOn' cs ""
                        | otherwise = splitOn' cs (c:sub)

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

debug :: String -> IO ()
debug = putStrLn

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

red, green, blue, cyan, black :: Color
black = 0
red = 1
green = 2
blue = 4
cyan = 6

--
-- * Run programs
--

runPrgNoFail_ :: FilePath -- ^ Executable
              -> [String] -- ^ Flags
              -> String   -- ^ Standard input
              -> IO ()
runPrgNoFail_ exe flags input = runPrgNoFail exe flags input >> return ()

runPrgNoFail :: FilePath -- ^ Executable
             -> [String] -- ^ Flag
             -> String   -- ^ Standard input
             -> IO (String,String) -- ^ stdout and stderr
runPrgNoFail exe flags input = do
  let c = showCommandForUser exe flags
  hPutStr stderr $ "Running " ++ c ++ "... "
  (s,out,err) <- readProcessWithExitCode exe flags input
  hPutStrLnExitCode s stderr "."
  case s of
    ExitFailure x -> do
      reportError exe ("with status " ++ show x) (nullMaybe input) (nullMaybe out) (nullMaybe err)
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

reportErrorColor :: Color
                 -> String         -- ^ command that failed
                 -> String         -- ^ how it failed
                 -> Maybe String   -- ^ given input
                 -> Maybe String   -- ^ stdout output
                 -> Maybe String   -- ^ stderr output
                 -> IO ()
reportErrorColor col c m i o e =
    do
    putStrLn $ color col $ c ++ " failed: " ++ m
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
            -> Maybe String   -- ^ given input
            -> Maybe String   -- ^ stdout output
            -> Maybe String   -- ^ stderr output
            -> IO ()
reportError = reportErrorColor red

--

empty :: String
empty = ""

inproc :: FilePath -> [String] -> String -> IO ()
inproc cmd args _ = runPrgNoFail_ cmd args ""

procStrictWithErr :: FilePath -> [String] -> String -> IO (ExitCode,String,String)
procStrictWithErr = readProcessWithExitCode

ls :: FilePath -> IO [FilePath]
ls d = map (d </>) <$> listDirectory d

pwd :: IO FilePath
pwd = getCurrentDirectory

cd :: FilePath -> IO ()
cd = setCurrentDirectory

basename :: FilePath -> String
basename = takeBaseName

stripEnd :: String -> String
stripEnd = dropWhileEnd isSpace

echo :: String -> IO ()
echo = putStrLn

was_failure :: String -> Bool
was_failure = ("ERROR" `isInfixOf`) . map toUpper

runGood :: FilePath -> (FilePath,String,String) -> IO (Sum Int)
runGood lab4 good = do
  let (file,mode,expect) = good
  echo $ color blue $ "--- " <> basename file <> " ---"
  echo $ "     Mode: " <> mode
  echo $ "Expecting: " <> expect
  (exitval,stripEnd -> result,_) <- procStrictWithErr lab4 [mode, file] empty
  goodpass <- if (exitval /= ExitSuccess) then do
                  echo $ color red "Error"
                  return 0
              else
                  if (result == expect) then do
                      echo $ "   Output: " ++ color green result
                      return 1
                  else do
                      echo $ "   Output: " ++ color red result
                      return 0
  echo ""
  return goodpass

runBad :: FilePath -> FilePath -> IO (Sum Int)
runBad lab4 bad = do
  echo $ color blue $ "xxx " <> basename bad <> " xxx"
  (_,stdout1,stderr1) <- procStrictWithErr lab4 ["-v", bad] empty
  (_,stdout2,stderr2) <- procStrictWithErr lab4 ["-n", bad] empty
  let result1 = stripEnd $ stdout1 <> stderr1
      result2 = stripEnd $ stdout2 <> stderr2
  echo $ "CBV: " <> result1
  echo $ "CBN: " <> result2
  badpass <- if was_failure result1 && was_failure result2 then
                     return 1
             else
                     return 0
  echo ""
  return badpass

data Options = Options { makeFlag        :: Bool
                       , testSuiteOption :: Maybe TestSuite }

disableMake :: Options -> Maybe Options
disableMake options = Just $ options { makeFlag = False }

addGood :: String -> Options -> Maybe Options
addGood (splitOn ',' -> [f,m,r]) options = Just $ options { testSuiteOption = Just $ maybe ([testCase],[]) (first (testCase:)) $ testSuiteOption options }
  where
    testCase = (f,'-':m,r)
addGood _                        _       = Nothing

addBad :: FilePath -> Options -> Maybe Options
addBad f options = Just $ options { testSuiteOption = Just $ maybe ([],[f]) (second (f:)) $ testSuiteOption options }

optDescr :: [OptDescr (Options -> Maybe Options)]
optDescr = [ Option []    ["no-make"] (NoArg  disableMake               ) "do not run make"
           , Option ['g'] ["good"]    (ReqArg addGood "FILE,MODE,RESULT") "good test case FILE, call-by-name or -value MODE, expected RESULT"
           , Option ['b'] ["bad"]     (ReqArg addBad  "FILE"            ) "bad test case FILE"
           ]

parseArgs :: [String] -> IO (FilePath,TestSuite)
parseArgs argv = case getOpt RequireOrder optDescr argv of
  (o,[codedir],[]) -> do
    let defaultOptions = Options True Nothing
    options <- maybe usage return $ foldM (&) defaultOptions o
    when (not $ makeFlag options) $ writeIORef doMake False
    let goodTests = [ ("good/001.hs",    "-v", "7"         )
                    , ("good/002.hs",    "-n", "5"         )
                    , ("good/003.hs",    "-v", "5050"      )
                    , ("good/004.hs",    "-v", "720"       )
                    , ("good/005.hs",    "-n", "0"         )
                    , ("good/006.hs",    "-v", "1073741824")
                    , ("good/007.hs",    "-v", "1"         )
                    , ("good/008.hs",    "-v", "210"       )
                    , ("good/008.hs",    "-n", "210"       )
                    , ("good/church.hs", "-v", "8"         )
                    , ("good/009.hs",    "-v", "131072"    )
                    , ("good/010.hs",    "-v", "1"         )
                    , ("good/010.hs",    "-n", "1"         )
                    , ("good/011.hs",    "-v", "1"         )
                    , ("good/011.hs",    "-n", "1"         )
                    , ("good/012.hs",    "-v", "0"         )
                    , ("good/013.hs",    "-v", "1"         )
                    , ("good/014.hs",    "-n", "33"        )
                    , ("good/015.hs",    "-v", "1"         )
                    , ("good/015.hs",    "-n", "1"         )
                    , ("good/ski.hs",    "-n", "16"        )
                    , ("good/016.hs",    "-v", "18"        )
                    , ("good/016.hs",    "-n", "18"        )
                    , ("good/017.hs",    "-v", "2"         )
                    , ("good/017.hs",    "-n", "2"         )
                    , ("good/018.hs",    "-v", "2"         )
                    , ("good/018.hs",    "-n", "2"         )
                    , ("good/019.hs",    "-v", "0"         )
                    , ("good/019.hs",    "-n", "0"         )
                    , ("good/shadow.hs", "-n", "1"         )
                    , ("good/shadow2.hs","-n", "1"         )
                    ]
        testSuite              = fromMaybe (goodTests,["bad"]) $ testSuiteOption options
        listHSFiles d          = filter (".hs" `isExtensionOf`) <$> ls d
        expandPath  f          = doesDirectoryExist f >>= \b -> if b then listHSFiles f else return [f]
        expandPathGood (f,m,r) = map (\ f' -> (f',m,r)) <$> expandPath f
    testSuite' <- mapTupleM (concatMapM expandPathGood) (concatMapM expandPath) testSuite
    return (codedir,testSuite')
  (_,_,_) -> do
    _ <- usage
    exitFailure

usage :: IO a
usage = do
  hPutStrLn stderr "Usage: progs-test-lab4 [--no-make] [-g|--good FILE,MODE,RESULT]... [-b|--bad FILE]..."
  hPutStrLn stderr "           path_to_solution" -- "The path to the directory where your solution is located"
  exitFailure

-- | In various contexts this is guessed incorrectly
setup :: IO ()
setup = hSetBuffering stdout LineBuffering

-- | Whether to run make
{-# NOINLINE doMake  #-}
doMake :: IORef Bool
doMake = unsafePerformIO $ newIORef True

type TestSuite = ([(FilePath,String,String)],[FilePath])

main :: IO ()
main = do
  setup

  testdir <- pwd
  (codedir,(goodTests,badTests)) <- parseArgs =<< getArgs
  let adjustPath f = if isRelative f then joinPath [testdir,f] else f
      goodTests'   = map (first3 adjustPath) goodTests
      badTests'    = map adjustPath          badTests
      lab4         = "." </> executable_name

  cd codedir
  domake <- readIORef doMake
  when domake $ inproc "make" [] empty

  let goodtot = length goodTests'
      badtot  = length badTests'
  goodpass <- mconcat <$> forM goodTests' (runGood lab4)
  badpass  <- mconcat <$> forM badTests'  (runBad  lab4)

  echo "### Summary ###"
  echo $ show (getSum goodpass) <> " of " <> show goodtot <> " good tests passed."
  echo $ show (getSum badpass)  <> " of " <> show badtot  <> " bad tests passed (approximate check, only checks if any error at all was reported)."
