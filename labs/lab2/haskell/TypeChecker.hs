{-# OPTIONS_GHC -Wno-unused-imports #-} -- Turn off unused import warning off in stub

module TypeChecker where

import Control.Monad

import Data.Map (Map)
import qualified Data.Map as Map

import CMM.Abs
import CMM.Print (printTree)

type AnnotatedProgram = Program -- TODO: choose your flavor of typed ASTs
type TypeError = String

typecheck :: Program -> Either TypeError AnnotatedProgram
typecheck p = return p
