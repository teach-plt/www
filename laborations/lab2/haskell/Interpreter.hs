module Interpreter where

import Control.Monad

import Data.Map (Map)
import qualified Data.Map as Map

import CMM.Abs
import CMM.Print
import CMM.ErrM

interpret :: Program -> IO ()
interpret p = putStrLn "no interpreter yet"
