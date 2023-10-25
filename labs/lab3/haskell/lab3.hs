-- | lab3: Compiler from C-- to JAVA .class file.

import System.Environment (getArgs, getExecutablePath)
import System.Exit        (exitFailure)
import System.FilePath    (dropExtension, replaceExtension, splitFileName, takeDirectory, (</>))
import System.Process     (callProcess)

import CMM.Par                  (pProgram, myLexer)
import qualified CMM.Abs   as A (Program)
import qualified Annotated as T (Program)
import qualified Compiler  as C (compile)
import TypeChecker              (typecheck)

-- | Main: read file passed by only command line argument and run compiler pipeline.

main :: IO ()
main = do
  args <- getArgs
  case args of
    [file] -> readFile file >>= parse >>= check >>= compile file
    _      -> do
      putStrLn "Usage: lab3 <SourceFile>"
      exitFailure

-- | Parse file contents into AST.

parse :: String -> IO A.Program
parse s =
  case pProgram (myLexer s) of
    Left err  -> do
      putStrLn "SYNTAX ERROR"
      putStrLn err
      exitFailure
    Right tree -> return tree

-- | Type check and return a type-annotated program.

check :: A.Program -> IO T.Program
check tree =
  case typecheck tree of
    Left err -> do
      putStrLn "TYPE ERROR"
      putStrLn err
      exitFailure
    Right tree' -> return tree'

-- | Compile and produce a .class file in the same directory as the source file.

compile :: FilePath -> T.Program -> IO ()
compile file tree = do
  -- The class name is the base name of the file.
  let (dir, name) = splitFileName file
  let classname   = dropExtension name
  -- Compiler produces content of .j file
  let jtext       = C.compile classname tree
  let jfile       = replaceExtension file "j"
  writeFile jfile jtext
  -- Call jasmin, but ask it to place .class file in dir
  -- rather than in the current directory.
  callJasmin ["-d", dir, jfile]

-- | Invoke the external `jasmin` assembler with the given args and wait for it to finish.

callJasmin :: [String] -> IO ()
callJasmin args = do
  -- The directory where lab3 resides.
  directory <- takeDirectory <$> getExecutablePath
  -- jasmin.jar should be in the same directory.
  let jasminPath = directory </> "jasmin.jar"
  -- Call jasmin.jar through java.
  callProcess "java" $ ["-jar", jasminPath] ++ args

  -- Note: If the `jasmin` executable is in your PATH, you can replace
  -- the lines above by the simpler:
  -- callProcess "jasmin" args
