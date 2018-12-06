{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns      #-}
-- Test suite for lab 4
import           Prelude                        hiding (FilePath)

import qualified Control.Foldl      as Fold
import qualified Data.List.NonEmpty as NonEmpty
import           Data.Monoid
import qualified Data.Text          as Text

import           Turtle                         hiding (echo, toText)
import qualified Turtle             as Turtle

toText :: FilePath -> Text
toText f = either id id $ Turtle.toText f

toLine :: FilePath -> Line
toLine = NonEmpty.head . textToLines . toText

echo :: MonadIO io => Text -> io ()
echo = printf (s % "\n")

was_failure :: Text -> Bool
was_failure = not . null . match (begins $ choice ["INTERPRETER ERROR", "java.lang.RuntimeException", "ERROR"])

runGood :: Text -> (FilePath,Text,Text) -> IO (Sum Int)
runGood lab4 good = do
  let (file,mode,expect) = good
  echo $ "\ESC[34m--- " <> toText (basename file) <> " ---\ESC[0m"
  echo $ "     Mode: " <> mode
  echo $ "Expecting: " <> expect
  (exitval,Text.stripEnd -> result) <- procStrict lab4 [mode, toText $ file] empty
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

runBad :: Text -> FilePath -> IO (Sum Int)
runBad lab4 bad = do
  echo $ "\ESC[34mxxx " <> toText (basename bad) <> " xxx\ESC[0m"
  (exitval1,stdout1,stderr1) <- procStrictWithErr lab4 ["-v", toText bad] empty
  (exitval2,stdout2,stderr2) <- procStrictWithErr lab4 ["-n", toText bad] empty
  let result1 = Text.stripEnd $ stdout1 <> stderr1
  let result2 = Text.stripEnd $ stdout1 <> stderr1
  echo $ "CBV: " <> result1
  echo $ "CBN: " <> result2
  badpass <- if was_failure result1 && was_failure result2 then
                     return 1
             else
                     return 0
  echo ""
  return badpass

args :: Parser FilePath
args = argPath "path_to_solution" "The path to the directory where your solution is located"

main = do
  testdir <- pwd
  codedir <- options "Lab 4 Test Suite" args
  let lab4 = toText $ codedir </> "lab4"

  cd codedir
  shell "make" empty

  let goodTests = select [ (testdir </> "good/001.hs",    "-v", "7"         )
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
      badTests  = mfilter (\f -> hasExtension f "hs") $ ls $ testdir </> "bad"
  (goodtot,goodpass) <- foldIO goodTests $ (,) <$> Fold.generalize Fold.length <*> Fold.sink (runGood lab4)
  (badtot ,badpass)  <- foldIO badTests  $ (,) <$> Fold.generalize Fold.length <*> Fold.sink (runBad  lab4)

  echo "### Summary ###"
  echo $ format d (getSum goodpass) <> " of " <> format d goodtot <> " good tests passed."
  echo $ format d (getSum badpass)  <> " of " <> format d badtot  <> " bad tests passed (approximate check, only checks if any error at all was reported)."
