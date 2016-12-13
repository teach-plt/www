-- Agda teaser in DAT151/DIT231 Programming Language Technology 2016
-- Andreas Abel (partially plagiarized from Ulf Norell's ICFP 2013 talk)
-- St. Lucia 2016-12-13


-- Prelude
------------------------------------------------------------------------

-- Basic types

data ℕ : Set where
  zero : ℕ
  suc  : (n : ℕ) → ℕ

data List (A : Set) : Set where
  []  : List A
  _∷_ : (x : A) (xs : List A) → List A

-- Hello World


-- Basic propositions
------------------------------------------------------------------------

-- Conjunction

record _×_ (A B : Set) : Set where

-- Implication

modusPonens : ∀{A B} → A × (A → B) → B
modusPonens = {!!}

-- Disjuction

data _⊎_ (A B : Set) : Set where

-- A tautology involving disjunction

taut : ∀{A B} → (A → B) → A ⊎ B → B
taut = {!!}

-- Truth

record ⊤ : Set where

-- Absurdity

data ⊥ : Set where

⊥-elim : {A : Set} → ⊥ → A
⊥-elim = {!!}

¬ : (A : Set) → Set
¬ A = A → ⊥

-- Contraposition

contra : ∀{A B} → (A → B) → ¬ B → ¬ A
contra = {!!}

-- Decidable propositions

data Dec (P : Set) : Set where

-- Equality

data _≡_ {A : Set} (x : A) : A → Set where
  refl : x ≡ x

_≢_ : {A : Set} (x y : A) → Set
x ≢ y = ¬ (x ≡ y)


-- Simply-typed lambda-calculus
------------------------------------------------------------------------

-- Types

data Ty : Set where
  base : Ty
  _⇒_ : (a b : Ty) → Ty

-- Type equality is propositional equality

-- Injectivity properties

⇒≠base : ∀{a b} → (a ⇒ b) ≡ base → ⊥
⇒≠base = {!!}

⇒injl : ∀{a b c d} → (a ⇒ c) ≡ (b ⇒ d) → a ≡ b
⇒injl = {!!}

⇒injr : ∀{a b c d} → (a ⇒ c) ≡ (b ⇒ d) → c ≡ d
⇒injr = {!!}

-- Deciding type equality

eqTy : (a b : Ty) → Dec (a ≡ b)
eqTy = {!!}

-- Raw de Bruijn terms

data Exp : Set where

-- Typing contexts

Cxt = List Ty


-- Bidirectional typing
------------------------------------------------------------------------

--  Well typed well-scoped variables
--
--  _⊢var_:_ ⊆ Cxt × ℕ × Ty
--
--                         Γ   ⊢var   x : a
--  vz --------------   vs ----------------
--     Γ,a ⊢var 0 : a      Γ,b ⊢var 1+x : a

data Var : (Γ : Cxt) (x : ℕ) (a : Ty) → Set where

-- Typing judgements
--
-- Inference  _⊢ne_:_ ⊆ Cxt × Exp × Ty  (neutral expressions)
-- Checking   _⊢nf_:_ ⊆ Cxt × Exp × Ty  (normal forms)
--
--       Γ ⊢var x : b                   Γ ⊢ne f : a ⇒ b   Γ ⊢nf e : a
-- neVar ------------------       neApp ------------------------------
--       Γ ⊢ne  (var x) : b             Γ ⊢ne (app f e) : b
--
--       Γ,a ⊢nf     e : b              Γ ⊢ne e : a
-- nfAbs ----------------------   nfNe  ------------
--       Γ   ⊢nf abs e : a ⇒ b          Γ ⊢nf e : a

mutual
  data Ne (Γ : Cxt) : (e : Exp) (b : Ty) → Set where

  data Nf (Γ : Cxt) : (e : Exp) (a : Ty) → Set where


-- Type checking algorithm
------------------------------------------------------------------------

-- Sound context lookup

data Lookup (Γ : Cxt) (x : ℕ) : Set where

lookupVar : ∀ Γ (x : ℕ) → Lookup Γ x
lookupVar = {!!}

-- Sound type checking

-- Result of type checking

data Infer (Γ : Cxt) (e : Exp) : Set where

data Check (Γ : Cxt) (e : Exp) (a : Ty) : Set where

-- Inference rules

-- Variable

inferVar : {!!}
inferVar = {!!}

-- Application

inferApp : {!!}
inferApp = {!!}

-- Checking rules

-- Neutrals

checkNe : {!!}
checkNe = {!!}

-- Abstraction

checkAbs : {!!}
checkAbs = {!!}

-- Sound type-checker

mutual
  check : ∀ Γ e a → Check Γ e a
  check Γ e a = {!!}

  infer : ∀ Γ e → Infer Γ e
  infer Γ e = {!!}
