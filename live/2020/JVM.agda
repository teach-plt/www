-- Programming Language Technology DAT151/DIT231
-- 2020-12-15 Agda (version ≥2.6.0)
-- From Dependent Types to Verified Compilation

-- See also: Idris, Lean, Coq

-- A correct compiler for Hutton's Razor

open import Relation.Binary.PropositionalEquality
open ≡-Reasoning

-- Data types and pattern matching
---------------------------------------------------------------------------

-- Example of a data type.

data List (A : Set) : Set where
  []  : List A
  _∷_ : (x : A) (xs : List A) → List A   -- \ : :

-- Type C-u C-x = in Emacs to get info about unicode char

-- Natural numbers in unary notation.

data Nat : Set where
  zero : Nat
  suc : (n : Nat) → Nat

{-# BUILTIN NATURAL Nat #-}

-- NATURAL gives us decimal notation for Nat.

five = suc 4

infixl 10 _+_
infixr  6 _∷_

-- Addition.

_+_ : (n m : Nat) → Nat
zero  + m = m
suc n + m = suc (n + m)

-- C-c C-l   load
-- C-c C-c   case split
-- C-c C-SPC give
-- C-c C-r   refine

plus-0 : (n : Nat) → n + 0 ≡ n
plus-0 zero    = refl
plus-0 (suc n) = cong suc (plus-0 n)

-- C-c C-, display goal and hyps
-- C-c C-. display goal and hyps and term
-- refl : n ≡ n
-- cong f : x ≡ y → f x ≡ f y

-- Indexed types: Fin, Vec
---------------------------------------------------------------------------

-- Bounded numbers: Fin n = { m | m < n }.

data Fin : Nat → Set where
  zero : {n : Nat} → Fin (suc n)
  suc  : {n : Nat} (i : Fin n) → Fin (suc n)

-- Example with hidden arguments.

three : Fin 5
three = suc {n = 4} (suc {3} (suc zero))

-- C-c C-=  show constraints
-- C-c C-s  solve

-- Vectors (length-indexed lists).
-- Vec : Set → Nat → Set

data Vec (A : Set) : Nat → Set where
  []  : Vec A zero
  _∷_ : {n : Nat} (x : A) (xs : Vec A n) → Vec A (suc n)


-- Automatic quantification over hidden arguments.

variable
  n : Nat
  A : Set

-- Reading an element of a vector.

-- lookup : {n : Nat} {A : Set} (i : Fin n) (xs : Vec A n) → A
lookup : (i : Fin n) (xs : Vec A n) → A
lookup zero    (x ∷ xs) = x
lookup (suc i) (x ∷ xs) = lookup i xs
-- lookup i [] = {!!} -- unreachable

-- Expressions and interpretation: "Hutton's razor"
---------------------------------------------------------------------------

-- We have a single type of (natural) numbers.

Num = Nat

-- Expressions over n variables.

-- A variable is denoted by its number x < n.

data Exp (n : Nat) : Set where
  var  : (x : Fin n)    → Exp n
  num  : (w : Num)      → Exp n
  plus : (e e' : Exp n) → Exp n

-- An environment maps each variable to its value.

Value = Num
Env   = Vec Value

-- Interpretation of expressions.

eval : (e : Exp n) (γ : Env n) → Value
eval (var x)     γ = lookup x γ
eval (num w)     γ = w
eval (plus e e') γ = eval e γ + eval e' γ

-- A fragment of JVM
---------------------------------------------------------------------------

StackSize = Nat  -- Current stack size
StoreSize = Nat  -- Local variable store limit
Addr = Fin       -- Local variable address
Word = Nat       -- Word (should be 32bit signed int, but we use Nat here)

-- JVM instructions.
--
-- n  = number of local variables
-- m  = stack size before instruction
-- m' = stack size after instruction

variable
  k l m m' : StackSize

data Ins (n : StoreSize) : (m m' : StackSize) → Set where
  load  : (a : Addr n) → Ins n m       (1 + m)  -- Load variable onto stack.
  add   :                Ins n (2 + m) (1 + m)  -- Add top stack elements.
  ldc   : (w : Word)   → Ins n m       (1 + m)  -- Load constant onto stack.
  pop   :                Ins n (1 + m) m        -- Remove top stack element.

-- Instruction sequences.
-- Note that for instruction concatenation, the index "l" has to match.

data Inss (n : StoreSize) (m : StackSize) : (m' : StackSize) → Set where
  []  : Inss n m m
  ins : Ins n m l → Inss n m l
  _∙_ : (i : Inss n m l) (is : Inss n l k) → Inss n m k

infixr 10 _∙_

-- Compilation of expressions
---------------------------------------------------------------------------

-- The code for an expression leaves one additional value on the stack.

compile : (e : Exp n) → Inss n m (suc m)
compile (var x)     = ins (load x)
compile (num w)     = ins (ldc w)
compile (plus e e') = compile e ∙ compile e' ∙ ins add


-- JVM small-step semantics
---------------------------------------------------------------------------

-- JVM machine state (simplified).

Store = Env
Stack = Vec Word

-- data State (n : StoreSize) (m : StackSize) : Set where
--   state : (V : Store n) (S : Stack m) → State n m

record State (n : StoreSize) (m : StackSize) : Set where
  constructor state
  field
    V  : Store n
    S  : Stack m
open State

-- Executing a JVM instruction.

step : (i : Ins n m m') (s : State n m) → State n m'
step (load a) (state V S)           = state V (lookup a V ∷ S)
step add      (state V (v ∷ w ∷ S)) = state V (v + w ∷ S)
step (ldc w)  (state V S)           = state V (w ∷ S)
step pop      (state V (_ ∷ S))     = state V S

-- Executing a series of JVM instructions.

steps : (is : Inss n m m') (s : State n m) → State n m'
steps (ins i)    s = step i s
steps []         s = s
steps (is ∙ is') s = steps is' (steps is s)

-- Compiler correctness
---------------------------------------------------------------------------

-- Pushing a word onto the stack.

push : Word → State n m → State n (suc m)
push w (state V S) = state V (w ∷ S)

push-eval : (e : Exp n) (s : State n m) → State n (suc m)
push-eval e (state V S) = state V (eval e V ∷ S)


-- Compiler correctness theorem.

-- Running the instructions for an expression e
-- leaves the value of e on top of the stack.

sound : (e : Exp n) (s : State n m) →

  steps (compile e) s ≡ push-eval e s

-- Case: variables
sound (var x)     s@(state V S) = begin
  steps (compile (var x)) s                   ≡⟨ refl ⟩
  steps (ins (load x)) s                      ≡⟨ refl ⟩
  step (load x) s                             ≡⟨ refl ⟩
  step (load x) (state V S)                   ≡⟨ refl ⟩
  state V (lookup x V ∷ S)                    ≡⟨ refl ⟩
  state V (eval (var x) V ∷ S)                ≡⟨ refl ⟩
  push-eval (var x) (state V S)               ≡⟨ refl ⟩
  push-eval (var x) s ∎

-- Case: number literals.
sound (num w)     s = refl
  -- C-u C-u C-c C-,

-- Case: addition expression.
sound (plus e e') s = begin
  steps (compile (plus e e')) s                         ≡⟨⟩
  steps (compile e ∙ compile e' ∙ ins add) s            ≡⟨⟩
  step add (steps (compile e') (steps (compile e) s))   ≡⟨ cong (λ □ → step add (steps (compile e') □)) (sound e s) ⟩
  step add (steps (compile e') (push-eval e s))         ≡⟨ {!!} ⟩
  push-eval (plus e e') s
  ∎
  where s' = push-eval e s

-- steps (compile (plus e e') s)
--   = steps (compile e ∙ compile e' ∙ ins add) s
--   = steps (compile e' ∙ ins add) (steps (compile e) s)  -- by ind.hyp.
--   = steps (compile e' ∙ ins add) (push-eval e s)
--   = steps (ins add) (steps (compile e') (push-eval e s))
--   = steps (ins add) (push-eval e' (push-eval e s))
--   = step add (push-eval e' (push-eval e s))
--   = step add (push-eval e' (push-eval e (state V S)))   -- s =: state V S
--   = step add (push-eval e' (state V (eval e V ∷ S)))
--   = step add (state V (eval e' V ∷ eval e V ∷ S))
--   = state V ((eval e V + eval e' V) ∷ S)
--   = state V (eval (plus e e') V ∷ S)
--   = push-eval (plus e e') (state V S)
--   = push-eval (plus e e') s

-- -}
-- -}
-- -}
-- -}
-- -}
-- -}
-- -}
-- -}
-- -}
-- -}
-- -}
-- -}
-- -}
-- -}
-- -}
-- -}
-- -}
