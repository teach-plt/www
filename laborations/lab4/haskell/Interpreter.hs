{-# OPTIONS_GHC -Wno-unused-imports #-} -- Turn off unused import warning off in stub
{-# OPTIONS_GHC -Wno-unused-matches #-} -- Turn off unused binding warning off in stub

-- | Interpreter for lambda-calculus with if, +, -, <.
--
--   Strategy can be either call-by-value or call-by-name.

module Interpreter (interpret, Strategy(..)) where

import Control.Applicative
import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Except

import Data.Functor
import Data.Map (Map)
import qualified Data.Map as Map

import Fun.Abs
import Fun.Print

-- | Evaluation strategy.

data Strategy
  = CallByName
  | CallByValue

-- | Error monad.

type Err = Except String

-- | Entry point: Program computes a number.

interpret :: Strategy -> Program -> Err Integer
interpret strategy (Prog defs (DMain mainExp)) = do
  throwError $ "TODO: implement interpreter"
