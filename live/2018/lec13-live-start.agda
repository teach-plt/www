-- Programming Language Technology DAT151/DIT231
-- 2018-12-18 Agda
-- From Dependent Types to Verified Compilation

-- A correct compiler for Hutton's Razor

-- {-# OPTIONS --postfix-projections #-}

-- open import Relation.Binary.PropositionalEquality
-- open ≡-Reasoning

-- Example of a data type.

data List (A : Set) : Set where

{-
-- Natural numbers in unary notation.

data Nat : Set where
  zero : Nat
  suc : (n : Nat) → Nat

-- {-# BUILTIN NATURAL Nat #-}

-- -- NATURAL gives us decimal notation for Nat.

-- five = suc 4

infixl 10 _+_
infixr  6 _∷_

-- Addition.

_+_ : (n m : Nat) → Nat
n + m = {!!}

-- Bounded numbers: Fin n = { m | n < m }.

data Fin : Nat → Set where

-- Example with hidden arguments.

-- three : Fin 5
-- three = suc {4} (suc {3} (suc {2} (zero {1})))

-- Vectors (length-indexed lists).
-- Vec : Set → Nat → Set

data Vec (A : Set) : Nat → Set where

-- Reading an element of a vector.

lookup : ∀{n A} (i : Fin n) (xs : Vec A n) → A
lookup i xs = {!!}

-- Expressions (Hutton's Razor only).

Var  = Fin
Word = Nat       -- Word (should be 32bit signed int, but we use Nat here)
Env  = Vec Word

data Exp (n : Nat) : Set where
  -- var
  -- num
  -- plus

-- Interpretation of expressions.

eval : ∀{n} (e : Exp n) (ρ : Env n) → Word
eval e ρ = {!!}

-- A partial formalization of JVM instructions and execution.

StackSize = Nat  -- Current stack size
StoreSize = Nat  -- Local variable store limit
Addr = Fin       -- Local variable address

-- JVM instructions.
--
-- n  = number of local variables
-- m  = stack size before instruction
-- m' = stack size after instruction

data Ins (n : StoreSize) : (m m' : StackSize) → Set where
  -- load    -- Load variable onto stack.
  -- ldc     -- Load constant onto stack.
  -- add     -- Add top stack elements.

-- Instruction sequences.
-- Note that for instruction concatenation, the index "l" has to match.

data Inss (n : StoreSize) (m : StackSize) : (m' : StackSize) → Set where
  []  : Inss n m m
  ins : ∀{l}   (i : Ins n m l)                      → Inss n m l
  _∙_ : ∀{l k} (is : Inss n m l) (is' : Inss n l k) → Inss n m k

infixr 10 _∙_

-- JVM machine state (simplified).

Stack = Vec Word

record State (n : StoreSize) (m : StackSize) : Set where
  constructor state
  field
    V  : Env n
    S  : Stack m
open State

-- Executing a JVM instruction.

step : ∀{n m m'} (i : Ins n m m') (s : State n m) → State n m'
step i  (state V S) = {!!}

-- Executing a series of JVM instructions.

steps : ∀{n m m'} (is : Inss n m m') (s : State n m) → State n m'
steps is s = {!!}

-- Compilation of expressions.

compile : ∀{n m} (e : Exp n) → Inss n m (suc m)
compile e = {!!}

-- Pushing a word onto the stack.

{-
push-eval : ∀{n m} (e : Exp n) (s : State n m) → State n (suc m)
push-eval e (state V S) = state V (eval e V ∷ S)

{-

-- Compiler correctness.

-- Theorem.
-- Running the instructions for an expression e leaves the value of e on top of the stack.

sound : ∀{n m} (e : Exp n) (s : State n m) →

  steps (compile e) s ≡ push-eval e s

-- Case: variables
sound (var x)     s = refl

-- Case: number literals.
sound (num w)     s = refl

-- Case: addition expression.
sound (plus e e') s = begin

  steps (compile e ∙ compile e' ∙ ins add) s  ≡⟨ cong (steps (compile e' ∙ ins add)) (sound e  s ) ⟩
  steps (compile e' ∙ ins add) s'             ≡⟨ cong (steps (ins add))              (sound e' s') ⟩
  steps (ins add) (push-eval e' s')           ≡⟨⟩
  (push-eval (plus e e') s)
  ∎
  where s' = push-eval e s



-- -}
-- -}
-- -}
-- -}
-- -}
