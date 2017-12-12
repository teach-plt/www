-- Programming Language Technology DAT151/DIT231
-- 2017-12-12 Agda
-- From Dependent Types to Verified Compilation

open import Relation.Binary.PropositionalEquality
open ≡-Reasoning

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

infixl 10 _+_
infixr  6 _∷_ _++_

-- Addition.

_+_ : (n m : Nat) → Nat
zero  + m = m
suc n + m = suc (n + m)

plus-0 : ∀(n : Nat) → n + 0 ≡ n
plus-0 zero    = {!!}
plus-0 (suc n) = {!!}

-- Bounded numbers: Fin n = { m | n < m }.

data Fin : Nat → Set where
  zero : {n : Nat} → Fin (suc n)
  suc  : {n : Nat} (i : Fin n) → Fin (suc n)

-- Example with hidden arguments.

three : Fin 5
three = suc {4} (suc {3} (suc {2} (zero {1})))

-- Vectors (length-indexed lists).
-- Vec : Set → Nat → Set

data Vec (A : Set) : Nat → Set where
  []  : Vec A zero
  _∷_ : {n : Nat} (x : A) (xs : Vec A n) → Vec A (suc n)

-- Reading an element of a vector.

lookup : ∀{n A} (i : Fin n) (xs : Vec A n) → A
lookup zero    (x ∷ xs) = x
lookup (suc i) (x ∷ xs) = lookup i xs

-- Changing and element of a vector.

update : ∀{n A} (i : Fin n) (x : A) (xs : Vec A n) → Vec A n
update zero x    (_ ∷ xs) = x ∷ xs
update (suc i) x (y ∷ xs) = y ∷ update i x xs

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

data Ins (n : StoreSize) : (m m' : StackSize) → Set where
  load  : ∀{m} (a : Addr n) → Ins n m       (1 + m)  -- Load variable onto stack.
  store : ∀{m} (a : Addr n) → Ins n (1 + m) m        -- Store top stack element into variable.
  add   : ∀{m}              → Ins n (2 + m) (1 + m)  -- Add top stack elements.
  ldc   : ∀{m} (w : Word)   → Ins n m       (1 + m)  -- Load constant onto stack.
  pop   : ∀{m} (w : Word)   → Ins n (1 + m) m        -- Remove top stack element.

-- JVM machine state (simplified).

record State (n : StoreSize) (m : StackSize) : Set where
  constructor state
  field
    V  : Vec Word n
    S  : Vec Word m
open State

-- Pushing a word onto the stack.

push : ∀{n m} → Word → State n m → State n (suc m)
push w (state V S) = state V (w ∷ S)

-- Executing a JVM instruction.

step : ∀{n m m'} (i : Ins n m m') (s : State n m) → State n m'
step (load a)  (state V S)           = state V (lookup a V ∷ S)
step (store a) (state V (w ∷ S))     = state (update a w V) S
step add       (state V (w ∷ v ∷ S)) = state V (v + w ∷ S)
step (ldc w)   (state V S)           = state V (w ∷ S)
step (pop w)   (state V (_ ∷ S))     = state V S

-- Instruction lists.
-- Note that for instruction cons, the index "l" has to match.

data Inss (n : StoreSize) (m : StackSize) : (m' : StackSize) → Set where
  []  : Inss n m m
  _∷_ : ∀{l k} (i : Ins n m l) (is : Inss n l k) → Inss n m k

-- Concatenating instruction lists.

_++_ : ∀{n m m' m''} → Inss n m m' → Inss n m' m'' → Inss n m m''
[]       ++ is' = is'
(i ∷ is) ++ is' = i ∷ (is ++ is')

-- Lemma: Appending and empty list.

++-[] : ∀{n m m'} (i : Inss n m m') → i ++ [] ≡ i
++-[] = {!!}

-- Lemma: Concatenation is associative.

++-assoc :  ∀{n m m' m'' m'''} (is : Inss n m m') (is' : Inss n m' m'') (is'' : Inss n m'' m''')
  → (is ++ is') ++ is'' ≡ is ++ (is' ++ is'')
++-assoc = {!!}

-- Executing a series of JVM instructions.

steps : ∀{n m m'} (is : Inss n m m') (s : State n m) → State n m'
steps []       s = s
steps (i ∷ is) s = steps is (step i s)

-- Executing a concatenation.

steps-++ : ∀{n m m' m''} (is : Inss n m m') (is' : Inss n m' m'') (s : State n m) →
  steps (is ++ is') s ≡ steps is' (steps is s)
steps-++ []       is' s = refl
steps-++ (i ∷ is) is' s = steps-++ is is' (step i s)

-- Expressions (Hutton's Razor only).

data Exp : Set where
  num  : (w : Word) → Exp
  plus : (e e' : Exp) → Exp

-- Interpretation of expressions.

eval : Exp → Word
eval (num w)     = w
eval (plus e e') = eval e + eval e'

-- Compilation of expressions.

compile : ∀{n m} (e : Exp) → Inss n m (suc m)
compile (num w)     = ldc w ∷ []
compile (plus e e') = compile e ++ compile e' ++ add ∷ []

-- Compiler correctness.

-- Lemma.
-- Running the instructions for an expression e leaves the value of e on top of the stack,
-- for a continuation to run from here.

sound' : ∀{n m m'} (e : Exp) (s : State n m) (is : Inss n (suc m) m') →

  steps (compile e ++ is) s ≡ steps is (push (eval e) s)

-- Case: number literals.
sound' (num w)     s is = steps-++ (ldc w ∷ []) is s

-- Case: addition expression.
sound' (plus e e') s is = begin

  steps ((compile e ++ compile e' ++ add ∷ []) ++ is) s                                  ≡⟨ {!!} ⟩

  steps (compile e ++ compile e' ++ (add ∷ []) ++ is) s                                  ≡⟨ sound' e s (compile e' ++ (add ∷ []) ++ is) ⟩

  steps              (compile e' ++ (add ∷ []) ++ is) (push (eval e) s)                  ≡⟨ sound' e' (push (eval e) s) ((add ∷ []) ++ is) ⟩

  steps                            ((add ∷ []) ++ is) (push (eval e') (push (eval e) s)) ≡⟨ steps-++ (add ∷ []) is (push (eval e') (push (eval e) s)) ⟩

  steps is (push (eval e + eval e') s)
  ∎

-- Theorem.
-- Running the instructions for an expression e leaves the value of e on top of the stack.

sound : ∀{n m} (e : Exp) (s : State n m) → steps (compile e) s ≡ push (eval e) s
sound {n} e s = begin
  steps (compile e) s        ≡⟨ cong (λ z → steps z s) (sym (++-[] (compile e))) ⟩
  steps (compile e ++ []) s  ≡⟨ sound' e s [] ⟩
  steps [] (push (eval e) s) ≡⟨⟩
  push (eval e) s
  ∎
