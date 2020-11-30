module Interpreter where

import Control.Monad

import Data.Map (Map)
import qualified Data.Map as Map

import CMM.Abs
import CMM.Print

import TypeChecker (AnnotatedProgram) -- TODO: choose flavor of typed ASTs

interpret :: AnnotatedProgram -> IO ()
interpret p = putStrLn "no interpreter yet"
