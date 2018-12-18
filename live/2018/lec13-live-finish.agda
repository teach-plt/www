-- Programming Language Technology DAT151/DIT231
-- 2018-12-18 Agda
-- From Dependent Types to Verified Compilation

-- A correct compiler for Hutton's Razor

-- {-# OPTIONS --postfix-projections #-}

open import Relation.Binary.PropositionalEquality
open ≡-Reasoning

-- Example of a data type.

data List (A : Set) : Set where
  []  : List A
  _∷_ : (x : A) (xs : List A) → List A                        -- \ : :
        -- A → List A → List A

infixr  6 _∷_

-- <A> Map<A>
-- forall a . a -> [a]

thrice : (A : Set) → ((a : A) → List A)
thrice A a = a ∷ (a ∷ (a ∷ []))

thrice' : {A : Set} (a : A) → List A
thrice' a = a ∷ (a ∷ (a ∷ []))
-- thrice' {A = B} a = a ∷ (a ∷ (a ∷ []))

-- Natural numbers in unary notation.

data Nat : Set where
  zero : Nat
  suc  : (n : Nat) → Nat

{-# BUILTIN NATURAL Nat #-}

-- NATURAL gives us decimal notation for Nat.

five = suc 4

infixl 10 _+_

-- Addition.

-- Load: C-c C-l
-- Give: C-c C-SPC
-- Hypotheses: C-c C-,
-- Cases:      C-c C-c

_+_ : (n m : Nat) → Nat
-- n     + zero    = n
-- zero  + (suc m) = suc m
-- suc n + (suc m) = suc (n + suc m)
zero  + m = m
suc n + m = suc (n + m)

plus-0 : ∀ n → n + 0 ≡ n
plus-0 zero    = refl
plus-0 (suc n) = cong suc (plus-0 n)

-- plus : (n m : Nat) → Nat
-- plus n zero    = n
-- plus n (suc m) = suc (plus n m)

test : Nat → Nat
test n = {! _+_ n zero  !}

-- C-c C-n  normalize

-- Vectors (length-indexed lists).
-- Vec : Set → Nat → Set

data Vec (A : Set) : Nat → Set where
  []  : Vec A zero
  _∷_ : {n : Nat} (x : A) (xs : Vec A n) → Vec A (suc n)

-- C-c C-.  hypotheses & hole
-- \ forall

-- _++_ : ∀ {A n} {m : _} (xs : Vec A n) (ys : Vec A m) → Vec A (plus n m)
-- xs ++ [] = xs
-- xs ++ (x ∷ ys) = {!x ∷ (xs ++ ys) !}

module ListAppend where

  _++_ : ∀ {A} (xs : List A) (ys : List A) → List A
  []       ++ ys = ys
  (x ∷ xs) ++ ys = {!!}

-- C-c C-a  auto
-- x ∷ (xs ++ ys)

_++_ : ∀ {A n} {m : _} (xs : Vec A n) (ys : Vec A m) → Vec A (n + m)
[]       ++ ys = ys
(x ∷ xs) ++ ys = x ∷ (xs ++ ys)

-- Bounded numbers: Fin n = { m | m < n }.

data Fin : (n : Nat) → Set where
  fzero : {n : Nat}             → Fin (suc n)  -- 0 < suc n
  fsuc  : {n : Nat} (i : Fin n) → Fin (suc n)  -- i < n     implies   suc i < suc n

{-
-- Example with hidden arguments.

-- three : Fin 5
-- three = suc {4} (suc {3} (suc {2} (zero {1})))
-}

-- Reading an element of a vector.
-- xs : Vec A n
-- i < n

lookup : ∀{n A} (i : Fin n) (xs : Vec A n) → A
lookup fzero    (x ∷ xs) = x
lookup (fsuc i) (x ∷ xs) = lookup i xs

v2 : Vec Nat 2
v2 = 3 ∷ 5 ∷ []

-- C-c C-= constraints
-- C-c C-s solve

i2 : Fin 4
i2 = fsuc (fsuc (fsuc (fzero {{!!}})))

-- test2 = lookup i2 v2


-- Expressions (Hutton's Razor only).

Var   = Fin

Value : Set
Value = Nat       -- Value (should be 32bit signed int, but we use Nat here)

-- Grammar for arithmethical expressions like (4 + x) + y
-- (4 + v0) + v1

-- n is number of variables we may use in the expression
-- E.g. n = 2

data Exp (n : Nat) : Set where
  var  : (x : Fin n)     → Exp n
  num  : (k : Value)      → Exp n
  plus : (e₁ e₂ : Exp n) → Exp n

-- Interpretation of expressions.

Env : (n : Nat) → Set
Env n = Vec Value n

eval : ∀{n} (e : Exp n) (ρ : Env n) → Value
eval (var x)      ρ = lookup x ρ
eval (num k)      ρ = k
eval (plus e₁ e₂) ρ = eval e₂ ρ + eval e₁ ρ

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
  load : ∀{m} (a : Addr n) → Ins n m       (1 + m)  -- Load variable onto stack.
  ldc  : ∀{m} (k : Value)  → Ins n m       (1 + m)  -- Load constant onto stack.
  add  : ∀{m}              → Ins n (2 + m) (1 + m)  -- Add top stack elements.

-- Instruction sequences.
-- Note that for instruction concatenation, the index "l" has to match.

data Inss (n : StoreSize) (m : StackSize) : (m' : StackSize) → Set where
  []  : Inss n m m
  ins : ∀{l}   (i  : Ins  n m l)                    → Inss n m l
  _∙_ : ∀{l k} (is : Inss n m l) (is' : Inss n l k) → Inss n m k

infixr 10 _∙_

-- JVM machine state (simplified).

Stack = Vec Value

record State (n : StoreSize) (m : StackSize) : Set where
  constructor state
  field
    V  : Env n     -- Vec Value n
    S  : Stack m   -- Vec Value m
open State

-- Executing a JVM instruction.

step : ∀{n m m'} (i : Ins n m m') (s : State n m) → State n m'
step (load a) (state V S)           = state V (lookup a V ∷ S)
step (ldc k)  (state V S)           = state V (k ∷ S)
step add      (state V (v ∷ w ∷ S)) = state V (v + w ∷ S)

-- C-c C-r refine

-- Executing a series of JVM instructions.

steps : ∀{n m m'} (is : Inss n m m') (s : State n m) → State n m'
steps [] s          = s
steps (ins i) s     = step i s
-- steps (is₁ ∙ is₂) s = steps {! is₁ !} (steps is₁ s)
steps (is₁ ∙ is₂) s = steps is₂ (steps is₁ s)

-- Compilation of expressions.

compile : ∀{n m} (e : Exp n) → Inss n m (1 + m)
compile (var x)      = ins (load x)
compile (num k)      = ins (ldc k)
compile (plus e₁ e₂) = compile e₁ ∙ compile e₂ ∙ ins add

-- Pushing a word onto the stack.

push-eval : ∀{n m} (e : Exp n) (s : State n m) → State n (suc m)
push-eval e (state V S) = state V (eval e V ∷ S)

-- Compiler correctness.

-- Theorem.
-- Running the instructions for an expression e leaves the value of e on top of the stack.

sound : ∀{n m} (e : Exp n) (s : State n m) →

  steps (compile e) s ≡ push-eval e s

{-
sound (var x) (state V S) = refl
sound (num k) (state V S) = refl
sound (plus e₁ e₂) s = {!!}
-}

-- Case: variables
sound (var x)     s = refl

-- Case: number literals.
sound (num k)     s = refl

-- Case: addition expression.
sound (plus e₁ e₂) s = begin

  steps (compile (plus e₁ e₂))              s  ≡⟨⟩
  steps (compile e₁ ∙ compile e₂ ∙ ins add) s  ≡⟨ cong (steps (compile e₂ ∙ ins add)) (sound e₁ s ) ⟩
  steps              (compile e₂ ∙ ins add) s₁ ≡⟨ cong (steps (ins add))              (sound e₂ s₁) ⟩
  steps                           (ins add) s₂ ≡⟨⟩
  push-eval (plus e₁ e₂) s
  ∎
  where
  s₁ = push-eval e₁ s   -- equals  steps (compile e₁) s   by  sound e₁ s
  s₂ = push-eval e₂ s₁  -- equals  steps (compile e₂) s₁  by  sound e₂ s₁

-- -}
-- -}
-- -}
-- -}
-- -}
