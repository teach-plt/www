{-# OPTIONS_GHC -Wno-unused-imports #-} -- Turn off unused import warning off in stub

-- | Type checker for C--, producing typed syntax from ASTs.

module TypeChecker where

import Control.Applicative
import Control.Monad
import Control.Monad.Except
import Control.Monad.Reader
import Control.Monad.State

import Data.Functor
import Data.Maybe
import Data.Map (Map)
import qualified Data.Map as Map
import qualified Data.Set as Set

import CMM.Abs
import CMM.Print (printTree)

import qualified Annotated as A

type TypeError = String

-- | Entry point of type checker.

typecheck :: Program -> Either TypeError A.Program
typecheck prg = return prg
