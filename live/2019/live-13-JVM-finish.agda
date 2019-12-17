-- Programming Language Technology DAT151/DIT231
-- 2019-12-17 Agda (version ≥2.6.0)
-- From Dependent Types to Verified Compilation

-- A correct compiler for Hutton's Razor

open import Relation.Binary.PropositionalEquality
open ≡-Reasoning

-- Example of a data type.

data List (A : Set) : Set where
  []  : List A
  _∷_ : (x : A) (xs : List A) → List A   -- \ : :

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

-- C-c C-l   load
-- C-c SPACE give
-- C-c C-c   case split

_+_ : (n m : Nat) → Nat
zero  + m = m
suc n + m = suc (n + m)

-- C-c C-.   give type of expression in hole

plus-0-l : ∀(n : Nat) → 0 + n ≡ n
plus-0-l n = refl

-- C-c C-,   give type of hole

plus-0-r : ∀(n : Nat) → n + 0 ≡ n
plus-0-r zero    = refl
plus-0-r (suc n) = cong suc (plus-0-r n)

-- Bounded numbers: Fin n = { m | m < n }.

data Fin : Nat → Set where
  zero : {n : Nat} → Fin (suc n)
  suc  : {n : Nat} (i : Fin n) → Fin (suc n)

-- Example with hidden arguments.

three : Fin 5
three = suc {4} (suc {3} (suc {2} (zero {1})))

-- C-c C-=
-- C-c C-s

-- Vectors (length-indexed lists).
-- Vec : Set → Nat → Set

data Vec (A : Set) : Nat → Set where
  []  : Vec A zero
  _∷_ : {n : Nat} (x : A) (xs : Vec A n) → Vec A (suc n)

-- Reading an element of a vector.

lookup : ∀{n A} (i : Fin n) (xs : Vec A n) → A
-- lookup ()      []
lookup zero    (x ∷ xs) = x
lookup (suc i) (x ∷ xs) = lookup i xs

variable
  A : Set
  -- n m : Nat

append : ∀{n m} (xs : Vec A n) (ys : Vec A m) → Vec A (n + m)
append []       ys = ys -- subst (Vec _) (sym (plus-0-r _)) ys
append (x ∷ xs) ys = x ∷ append xs ys

-- A partial formalization of JVM instructions and execution.

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
  n        : StoreSize
  k l m m' : StackSize

data Ins (n : StoreSize) : (m m' : StackSize) → Set where
  load  : (a : Addr n) → Ins n m       (1 + m)  -- Load variable onto stack.
  add   :                Ins n (2 + m) (1 + m)  -- Add top stack elements.
  ldc   : (w : Word)   → Ins n m       (1 + m)  -- Load constant onto stack.
  pop   :                Ins n (1 + m) m        -- Remove top stack element.

-- Instruction sequences.
-- Note that for instruction concatenation, the index "l" has to match.

data Inss (n : StoreSize) (m : StackSize) : (m' : StackSize) → Set where
  []  :                                        Inss n m m
  ins : (i : Ins n m l)                      → Inss n m l
  _∙_ : (is : Inss n m l) (is' : Inss n l k) → Inss n m k

infixr 10 _∙_

-- Expressions (Hutton's Razor only).

data Exp (n : Nat) : Set where
  var  : (x : Addr n)   → Exp n
  num  : (w : Word)     → Exp n
  plus : (e e' : Exp n) → Exp n

-- Compilation of expressions.

compile : (e : Exp n) → Inss n m (1 + m)
compile (var x)     = ins (load x)
compile (num w)     = ins (ldc w)
compile (plus e e') = compile e ∙ compile e' ∙ ins add

-- JVM machine state (simplified).

Env   = Vec Word
Stack = Vec Word

record State (n : StoreSize) (m : StackSize) : Set where
  constructor state
  field
    V  : Env n     -- local variable store
    S  : Stack m   -- stack
open State

-- Interpretation of expressions.

eval : (e : Exp n) (ρ : Env n) → Word
eval (var x)     ρ = lookup x ρ
eval (num w)     ρ = w
eval (plus e e') ρ = eval e ρ + eval e' ρ

-- Executing a JVM instruction.

step : (i : Ins n m m') (s : State n m) → State n m'
step (load a) (state V S          ) = state V (lookup a V ∷ S)
step add      (state V (w ∷ v ∷ S)) = state V (v + w ∷ S)
step (ldc w)  (state V S          ) = state V (w ∷ S)
step pop      (state V (_ ∷ S)    ) = state V S

-- Executing a series of JVM instructions.

steps : (is : Inss n m m') (s : State n m) → State n m'
steps (ins i)    s = step i s
steps []         s = s
steps (is ∙ is') s = steps is' (steps is s)

-- Pushing a word onto the stack.

push : Word → State n m → State n (suc m)
push w (state V S) = state V (w ∷ S)

push-eval : (e : Exp n) (s : State n m) → State n (suc m)
push-eval e (state V S) = state V (eval e V ∷ S)

-- Compiler correctness.

-- Theorem.
-- Running the instructions for an expression e leaves the value of e on top of the stack.

sound : (e : Exp n) (s : State n m) →

  steps (compile e) s ≡ push-eval e s

-- Case: variables
sound (var x)     s = refl
  -- steps (compile (var x)) s
  --   = steps (ins (load x)) s
  --   = step (load x) s             -- s = state V S
  --   = step (load x) (state V S)
  --   = state V (lookup x V ∷ S)
  --   = state V (eval (var x) V ∷ S)
  --   = push-eval (var x) (state V S)
  --   = push-eval (var x) s

-- Case: number literals.
sound (num w)     s@(state V S) = begin
  steps (compile (num w)) s
    ≡⟨⟩ steps (ins (ldc w)) s
    ≡⟨⟩ step (ldc w) s
    ≡⟨⟩ step (ldc w) (state V S)
    ≡⟨⟩ state V (w ∷ S)
    ≡⟨⟩ state V (eval (num w) V ∷ S)
    ≡⟨⟩ push-eval (num w) (state V S)
    ≡⟨⟩ push-eval (num w) s
    ∎

-- sound (num w)     s = refl
--   -- steps (compile (num w)) s
--   --   = steps (ins (ldc w)) s
--   --   = step (ldc w) s          -- s = state V S
--   --   = step (ldc w) (state V S)
--   --   = state V (w ∷ S)
--   --   = state V (eval (num w) V ∷ S)
--   --   = push-eval (num w) (state V S)
--   --   = push-eval (num w) s

-- Case: addition expression.
sound (plus e e') s@(state V S) = begin
  steps (compile (plus e e')) s                      ≡⟨ refl ⟩
  steps (compile e ∙ compile e' ∙ ins add) s         ≡⟨ refl ⟩
  steps (compile e' ∙ ins add) (steps (compile e) s) ≡⟨ cong (steps (compile e' ∙ ins add)) (sound e s) ⟩
  steps (compile e' ∙ ins add) (push-eval e s)       ≡⟨ refl ⟩
  steps (compile e' ∙ ins add) s'                    ≡⟨ refl ⟩
  steps (ins add) (steps (compile e') s')            ≡⟨ refl ⟩
  step add (steps (compile e') s')                   ≡⟨ cong (step add) (sound e' s') ⟩
  step add (push-eval e' s')                         ≡⟨ refl ⟩
  step add (push-eval e' (push-eval e (state V S)))  ≡⟨ refl ⟩
  step add (push-eval e' (state V (eval e V ∷ S)))   ≡⟨ refl ⟩
  step add (state V (eval e' V ∷ eval e V ∷ S))      ≡⟨ refl ⟩
  state V (eval e V + eval e' V ∷ S)                 ≡⟨ refl ⟩
  state V (eval (plus e e') V ∷ S)                   ≡⟨ refl ⟩
  push-eval (plus e e') s
  ∎
  where s' = push-eval e s


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
