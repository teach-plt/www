{-# LANGUAGE CPP               #-}
{-# LANGUAGE ViewPatterns      #-}
-- Test suite for lab 4
import Control.Monad

import Data.Char
import Data.List
import Data.Monoid

import System.Directory
import System.Environment
import System.Exit
import System.FilePath
import System.IO
import System.Process

-- Executable name
executable_name :: FilePath
-- You might have to add or remove .exe here if you are using Windows
#if defined(mingw32_HOST_OS)
executable_name = "lab4.exe"
#else
executable_name = "lab4"
#endif

debug :: String -> IO ()
debug = putStrLn

--
-- * Terminal output colors
--

type Color = Int

color :: Color -> String -> String
color c s = fgcol c ++ s ++ normal

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
              -> String   -- ^ Standard input
              -> IO ()
runPrgNoFail_ exe flags input = runPrgNoFail exe flags input >> return ()

runPrgNoFail :: FilePath -- ^ Executable
             -> [String] -- ^ Flag
             -> String   -- ^ Standard input
             -> IO (String,String) -- ^ stdout and stderr
runPrgNoFail exe flags input = do
  let c = showCommandForUser exe flags
  hPutStrLn stderr $ "Running " ++ c ++ "..."
  (s,out,err) <- readProcessWithExitCode exe flags input
  case s of
    ExitFailure x -> do
      reportError exe ("with status " ++ show x) input out err
      exitFailure
    ExitSuccess -> do
      debug $ "Standard output:\n" ++ out
      debug $ "Standard error:\n" ++ err
      return (out,err)

--
-- * Error reporting and output checking
--

reportErrorColor :: Color
                 -> String -- ^ command that failed
                 -> String -- ^ how it failed
                 -> String -- ^ given input
                 -> String -- ^ stdout output
                 -> String -- ^ stderr output
                 -> IO ()
reportErrorColor col c m i o e =
    do
    putStrLn $ color col $ c ++ " failed: " ++ m
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
            -> String -- ^ given input
            -> String -- ^ stdout output
            -> String -- ^ stderr output
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
  echo $ "\ESC[34m--- " <> basename file <> " ---\ESC[0m"
  echo $ "     Mode: " <> mode
  echo $ "Expecting: " <> expect
  (exitval,stripEnd -> result,_) <- procStrictWithErr lab4 [mode, file] empty
  goodpass <- if (exitval /= ExitSuccess) then do
                  echo "\ESC[31mError\ESC[0m"
                  return 0
              else
                  if (result == expect) then do
                      echo $ "   Output: \ESC[32m" <> result <> "\ESC[0m"
                      return 1
                  else do
                      echo $ "   Output: \ESC[31m" <> result <> "\ESC[0m"
                      return 0
  echo ""
  return goodpass

runBad :: FilePath -> FilePath -> IO (Sum Int)
runBad lab4 bad = do
  echo $ "\ESC[34mxxx " <> basename bad <> " xxx\ESC[0m"
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

parseArgs :: [String] -> IO FilePath
parseArgs [d] = return d
parseArgs _   = usage

usage :: IO a
usage = do
  hPutStrLn stderr "Usage: progs-test-lab4 path_to_solution" -- "The path to the directory where your solution is located"
  exitFailure

main :: IO ()
main = do
  testdir <- pwd
  codedir <- parseArgs =<< getArgs
  let lab4 = codedir </> executable_name

  cd codedir
  inproc "make" [] empty

  let goodTests = [ (testdir </> "good/001.hs",    "-v", "7"         )
                  , (testdir </> "good/002.hs",    "-n", "5"         )
                  , (testdir </> "good/003.hs",    "-v", "5050"      )
                  , (testdir </> "good/004.hs",    "-v", "720"       )
                  , (testdir </> "good/005.hs",    "-n", "0"         )
                  , (testdir </> "good/006.hs",    "-v", "1073741824")
                  , (testdir </> "good/007.hs",    "-v", "1"         )
                  , (testdir </> "good/church.hs", "-v", "8"         )
                  , (testdir </> "good/009.hs",    "-v", "131072"    )
                  , (testdir </> "good/010.hs",    "-v", "1"         )
                  , (testdir </> "good/010.hs",    "-n", "1"         )
                  , (testdir </> "good/011.hs",    "-v", "1"         )
                  , (testdir </> "good/011.hs",    "-n", "1"         )
                  , (testdir </> "good/012.hs",    "-v", "0"         )
                  , (testdir </> "good/013.hs",    "-v", "1"         )
                  , (testdir </> "good/014.hs",    "-n", "33"        )
                  , (testdir </> "good/015.hs",    "-v", "1"         )
                  , (testdir </> "good/015.hs",    "-n", "1"         )
                  , (testdir </> "good/ski.hs",    "-n", "16"        )
                  ]
  badTests <- filter (".hs" `isExtensionOf`) <$> (ls $ testdir </> "bad")
  let goodtot = length goodTests
      badtot  = length badTests
  goodpass <- mconcat <$> forM goodTests (runGood lab4)
  badpass  <- mconcat <$> forM badTests  (runBad  lab4)

  echo "### Summary ###"
  echo $ show (getSum goodpass) <> " of " <> show goodtot <> " good tests passed."
  echo $ show (getSum badpass)  <> " of " <> show badtot  <> " bad tests passed (approximate check, only checks if any error at all was reported)."
