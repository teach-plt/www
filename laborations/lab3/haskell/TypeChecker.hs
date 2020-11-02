{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase       #-}
{-# LANGUAGE PatternSynonyms  #-}
{-# LANGUAGE TupleSections    #-}

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
import CMM.ErrM  (Err, pattern Bad, pattern Ok)

import qualified Annotated as A

-- | Entry point of type checker.

typecheck :: Program -> Err A.Program
typecheck prg = Bad "Type checker not yet implemented"
