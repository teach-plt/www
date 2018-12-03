-- | lab3: Compiler from C-- to JAVA .class file.

import System.Environment (getArgs)
import System.Exit        (exitFailure)
import System.FilePath    (dropExtension, replaceExtension, splitFileName)
import System.Process     (callProcess)

import CPP.Par                  (pProgram, myLexer)
import CPP.ErrM                 (Err (Bad, Ok))
import qualified CPP.Abs   as A (Program)
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
    Bad err  -> do
      putStrLn "SYNTAX ERROR"
      putStrLn err
      exitFailure
    Ok  tree -> return tree

-- | Type check and return a type-annotated program.

check :: A.Program -> IO T.Program
check tree =
  case typecheck tree of
    Bad err -> do
      putStrLn "TYPE ERROR"
      putStrLn err
      exitFailure
    Ok tree' -> return tree'

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
  callProcess "jasmin" ["-d", dir, jfile]
