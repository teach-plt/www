{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TupleSections #-}

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

import CPP.Abs
import CPP.Print (printTree)
import CPP.ErrM  (Err(Ok, Bad))

import qualified Annotated as A

-- | Entry point of type checker.

typecheck :: Program -> Err A.Program
typecheck prg = Bad "Type checker not yet implemented"
