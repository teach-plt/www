-- Programming Language Technology (Chalmers DAT 151, GU DIT 231)
-- (C) Andreas Abel, 2023
-- All rights reserved.

module Interpreter where

open import Library
open import TypedSyntax

-- Entry point: interpret program using native I/O interaction.

runProgram : (prg : Program) → IO ⊤
runProgram prg = putStrLn "INTERPRETER ERROR: not yet implemented"
