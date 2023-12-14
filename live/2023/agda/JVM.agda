-- Programming Language Technology DAT151/DIT231
-- 2023-12-12 Agda (version ≥2.6.0)
-- From Dependent Types to Verified Compilation

-- A correct compiler for Hutton's Razor

open import Relation.Binary.PropositionalEquality
open ≡-Reasoning

-- Data types and pattern matching
---------------------------------------------------------------------------

-- Example of a data type.

data List (A : Set) : Set where
  []  : List A
  _∷_ : (x : A) (xs : List A) → List A

-- Natural numbers in unary notation.

data Nat : Set where
  zero : Nat
  suc : (n : Nat) → Nat

{-# BUILTIN NATURAL Nat #-}

-- NATURAL gives us decimal notation for Nat.

five = suc 4
-- five = suc (suc (suc (suc (suc zero))))

infixl 10 _+_
infixr  6 _∷_

-- Addition.

_+_ : (n m : Nat) → Nat
zero  + m = m
suc n + m = suc (n + m)

length : {A : Set} → List A → Nat
length []       = zero
length (x ∷ xs) = suc (length xs)

plus-0' : ∀(n : Nat) → 0 + n ≡ n
plus-0' n = refl


plus-0 : ∀(n : Nat) → n + 0 ≡ n
plus-0 zero = refl
plus-0 (suc n) rewrite plus-0 n = refl

-- Indexed types: Fin, Vec
---------------------------------------------------------------------------

-- Bounded numbers: Fin n = { m | n > m }.

data Fin : Nat → Set where
  zero : {n : Nat} → Fin (suc n)
  suc  : {n : Nat} (i : Fin n) → Fin (suc n)

toNat : (n : Nat) → Fin n → Nat
toNat .(suc _) zero = zero
toNat (suc n) (suc i) = suc (toNat n i)

toNat' : {n : Nat} → Fin n → Nat
toNat' zero    = zero
toNat' (suc i) = suc (toNat' i)


-- Example with hidden arguments.

three : Fin 5
three = suc {n = 4} (suc {3} (suc zero))

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

lookup : -- ∀ {n} {A : _}
  (i : Fin n) (xs : Vec A n) → A
-- lookup : ∀{n A} (i : Fin n) (xs : Vec A n) → A
-- lookup () []
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

-- An environment maps each variable to its value.

Value = Num

Env : (n : Nat) → Set
Env n = Vec Num n


-- Interpretation of expressions.

eval : (e : Exp n) (γ : Env n) → Value
eval (var x)      γ = lookup x γ
eval (num w)      γ = w
eval (plus e₁ e₂) γ = eval e₁ γ + eval e₂ γ

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
  _∙_ : (is : Inss n m l) (js : Inss n l k) → Inss n m k

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

record State (n : StoreSize) (m : StackSize) : Set where
  constructor state
  field
    V  : Store n
    S  : Stack m
open State

-- Executing a JVM instruction.

push : Word → State n m → State n (suc m)
push w (state V S) = state V (w ∷ S)

step : (i : Ins n m m') (s : State n m) → State n m'
step (load a) s                       = push (lookup a (s .V)) s
step add      (state V (x₂ ∷ x₁ ∷ S)) = state V (x₁ + x₂ ∷ S)
step (ldc w)  s                       = push w s
step pop      (state V (_ ∷ S))       = state V S

-- Compiler correctness
---------------------------------------------------------------------------

-- Executing a series of JVM instructions.

steps : (is : Inss n m m') (s : State n m) → State n m'
steps (ins i)    s = step i s
steps []         s = s
steps (is ∙ is') s = steps is' (steps is s)

-- Pushing a word onto the stack.

push-eval : (e : Exp n) (s : State n m) → State n (suc m)
push-eval e (state V S) = state V (eval e V ∷ S)

-- Compiler correctness theorem.

-- Running the instructions for an expression e
-- leaves the value of e on top of the stack.

sound : (e : Exp n) (s : State n m) →

  steps (compile e) s ≡ push-eval e s

-- Case: variables
-- sound (var x)     s = refl
sound (var x)     s = begin
   steps (compile (var x)) s      ≡⟨ refl ⟩
   steps (ins (load x)) s         ≡⟨ refl ⟩
   step  (load x) s               ≡⟨⟩
   push  (lookup x (s .V)) s      ≡⟨⟩
   push  (eval (var x) (s .V)) s  ≡⟨⟩
   push-eval (var x) s            ∎

-- Case: number literals.
sound (num w)     s = refl

-- Case: addition expression.
sound (plus e e') s = begin
  steps (compile (plus e e')) s                              ≡⟨⟩
  steps (compile e ∙ compile e' ∙ ins add) s                 ≡⟨⟩
  steps (compile e' ∙ ins add) (steps (compile e) s)         ≡⟨⟩
  steps (ins add) (steps (compile e') (steps (compile e) s)) ≡⟨ cong (λ z → steps (ins add) (steps (compile e') z)) (sound e s) ⟩
  steps (ins add) (steps (compile e') (push-eval e s))       ≡⟨ cong (steps (ins add)) (sound e' (push-eval e s)) ⟩
  steps (ins add) (push-eval e' (push-eval e s))             ≡⟨ refl ⟩
  push (eval e (s .V) + eval e' (s .V)) s                    ≡⟨⟩
  push (eval (plus e e') (s .V)) s                           ≡⟨⟩
  push-eval (plus e e') s
  ∎
  where s' = push-eval e s

-- -- Case: addition expression.
-- sound (plus e e') s = begin
--   steps (compile (plus e e')) s                              ≡⟨⟩
--   steps (compile e ∙ compile e' ∙ ins add) s                 ≡⟨⟩
--   steps (compile e' ∙ ins add) (steps (compile e) s)         ≡⟨⟩
--   steps (ins add) (steps (compile e') (steps (compile e) s)) ≡⟨  aux (sound e s)  ⟩
--   steps (ins add) (steps (compile e') (push-eval e s))       ≡⟨ cong (steps (ins add)) (sound e' (push-eval e s)) ⟩
--   steps (ins add) (push-eval e' (push-eval e s))             ≡⟨ refl ⟩
--   push (eval e (s .V) + eval e' (s .V)) s                    ≡⟨⟩
--   push (eval (plus e e') (s .V)) s                           ≡⟨⟩
--   push-eval (plus e e') s
--   ∎
--   where
--   s' = push-eval e s
--   aux : ∀ {s''} → steps (compile e) s ≡ s''  →
--       step add (steps (compile e') (steps (compile e) s)) ≡
--       step add (steps (compile e') s'')
--   aux refl = refl

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
-- -}
-- -}
