-- Programming Language Technology DAT151/DIT231
-- 2024-12-17 Agda (version ≥2.6.0)
-- From Dependent Types to Verified Compilation

-- A correct compiler for Hutton's Razor to a stack machine

-- Similar to an early pen-and-paper proof (register machine):
--
--   John McCarthy and James Painter, 1967
--   Correctness of a Compiler for Arithmetic Expressions
--   http://jmc.stanford.edu/articles/mcpain/mcpain.pdf

open import Relation.Binary.PropositionalEquality
open ≡-Reasoning

-- Data types and pattern matching
---------------------------------------------------------------------------

-- Example of a data type.

data List (A : Set) : Set where
  []  : List A
  _∷_ : (x : A) (xs : List A) → List A   -- \ : :   A → List A → List A

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
zero + m = m
suc n + m = suc (n + m)

{-
data Empty : Set where

inh : Empty
inh = error "hoo"
-}

plus-0 : (n : Nat) → n + zero ≡ n    --  \ = =
plus-0 zero = refl {x = 0}
plus-0 (suc n) = cong suc (plus-0 n)

-- Indexed types: Fin, Vec
---------------------------------------------------------------------------

-- Bounded numbers: Fin n = { m | m < n }.

data Fin : Nat → Set where
  zero : {n : Nat} → Fin (suc n)
  suc  : {n : Nat} (i : Fin n) → Fin (suc n)

-- Example with hidden arguments.

three : Fin 5
three = suc {n = 4} (suc (suc zero))

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

lookup : {n : Nat} {A : Set} (i : Fin n) (xs : Vec A n) → A
lookup zero    (x ∷ xs) = x
lookup (suc i) (x ∷ xs) = lookup i xs

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

-- (x + 3) + y
ex1 : Exp 2
ex1 = plus (plus (var zero) (num 3)) (var (suc zero))

-- An environment maps each variable to its value.

Value = Num
Env   = Vec Num

-- Interpretation of expressions.

eval : (e : Exp n) (γ : Env n) → Value
eval (var x) γ = lookup x γ
eval (num w) γ = w
eval (plus e e₁) γ = eval e γ + eval e₁ γ

v1 = eval ex1 (15 ∷ 1 ∷ [])

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

ss1 : Inss 2 0 1
ss1 = compile ex1

-- JVM small-step semantics
---------------------------------------------------------------------------

-- JVM machine state (simplified).

Store = Env
Stack = Vec Word

record State (n : StoreSize) (m : StackSize) : Set where
  constructor state
  field
    V  : Store n
    S  : Stack m
open State

-- Executing a JVM instruction.

step : (i : Ins n m m') (s : State n m) → State n m'
step (load a) (state V S)           = state V (lookup a V ∷ S)
step add      (state V (v ∷ w ∷ S)) = state V (w + v ∷ S)
step (ldc w)  (state V S)           = state V (w ∷ S)
step pop      (state V (_ ∷ S))     = state V S

-- Compiler correctness
---------------------------------------------------------------------------

-- Executing a series of JVM instructions.

steps : (is : Inss n m m') (s : State n m) → State n m'
steps (ins i)    s = step i s
steps []         s = s
steps (is ∙ is') s = steps is' (steps is s)

r1 = steps ss1 (state (15 ∷ 1 ∷ []) [])

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
sound (var x)     s = refl

-- Case: number literals.
sound (num w)     s = refl

-- Case: addition expression.
sound (plus e e') s = begin
  steps (compile (plus e e')) s                       ≡⟨ refl ⟩
  steps (compile e ∙ compile e' ∙ ins add) s          ≡⟨ refl ⟩
  steps (compile e' ∙ ins add) (steps (compile e) s)  ≡⟨ cong (steps _) (sound e s) ⟩
  steps (compile e' ∙ ins add) s'                     ≡⟨ refl  ⟩
  steps (ins add) (steps (compile e') s')             ≡⟨ cong (steps _) (sound e' s) ⟩
  steps (ins add) (push-eval e' s')                   ≡⟨ {!   !} ⟩
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
