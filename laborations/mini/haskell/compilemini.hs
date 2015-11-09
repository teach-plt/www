import System.Environment (getArgs)
import System.Exit (exitFailure)
import System.Process (callCommand)

import AbsMini
import LexMini
import ParMini
import ErrM

import AnnotatingTypeChecker
import Compiler


-- driver

comp :: String -> String -> IO ()
comp name s = case pProgram (myLexer s) of
            Bad err  -> do putStrLn "SYNTAX ERROR"
                           putStrLn err
                           exitFailure
            Ok  tree -> case typecheck tree of
                          Bad err -> do putStrLn "TYPE ERROR"
                                        putStrLn err
                                        exitFailure
                          Ok tree' -> do
                             writeFile (name ++ ".j") $ compile name tree'
                             putStrLn $ "wrote " ++ name ++ ".j"

main :: IO ()
main = do args <- getArgs
          case args of
            [file] -> do
               s <- readFile file
               comp "Foo" s               --- name Foo instead of (takeWhile (/='.') file)
               callCommand "java -jar jasmin.jar Foo.j"
               return ()
            _      -> do putStrLn "Usage: lab3 <SourceFile>"
                         exitFailure
