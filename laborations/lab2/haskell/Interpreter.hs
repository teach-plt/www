module Interpreter where

import Control.Monad

import Data.Map (Map)
import qualified Data.Map as Map

import AbsCPP     -- OR: import CPP.Abs
import PrintCPP   -- OR: import CPP.Print
import ErrM       -- OR: import CPP.ErrM

interpret :: Program -> IO ()
interpret p = putStrLn "no interpreter yet"
