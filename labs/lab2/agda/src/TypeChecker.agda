-- Programming Language Technology (Chalmers DAT 151, GU DIT 231)
-- (C) Andreas Abel, 2023
-- All rights reserved.

module TypeChecker where

open import Data.String using (_≟_)
open import Library

-- Use qualifiers A and T for abstract and typed syntax.
import CMM.AST     as A
import TypedSyntax as T

-- Type errors
------------------------------------------------------------------------

data TypeError : Set where
  notYetImplemented : _

printError : TypeError → String
printError = λ where
  notYetImplemented → "not yet implemented"

-- Checking the program
------------------------------------------------------------------------

module _ where
  open ErrorMonad {E = TypeError}

  checkProgram : (prg : A.Program) → Error T.Program
  checkProgram prg = fail notYetImplemented
