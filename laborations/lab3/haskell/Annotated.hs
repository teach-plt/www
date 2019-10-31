{-# LANGUAGE LambdaCase #-}

-- | Typed syntax for C--.

module Annotated where

import qualified CMM.Abs as A

-- This is a stub for typed ASTs produced by the type checker
-- as input for the compiler.

-- To make the stub compile, we just define an alias to
-- untyped ASTs here.

type Program = A.Program
