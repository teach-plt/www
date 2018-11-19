import Control.Monad
import System.Directory

import Data.Char (isSpace)

trim :: String -> String
trim = f . f
   where f = reverse . dropWhile isSpace

main :: IO ()
main = listDirectory "." >>= mapM_ delIfEmpty

--delIfEmpty :: FilePath -> IO ()
--delIfEmpty f = do
--  c <- readFile f
--  when (trim c == "") $ removeFile f
