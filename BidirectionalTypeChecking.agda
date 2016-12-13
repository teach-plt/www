-- Agda teaser in DAT151/DIT231 Programming Language Technology 2016
-- Andreas Abel (partially plagiarized from Ulf Norell's ICFP 2013 talk)
-- St. Lucia 2016-12-13


-- Prelude
------------------------------------------------------------------------

-- Basic types

data ℕ : Set where
  zero : ℕ
  suc  : (n : ℕ) -> ℕ

{-# BUILTIN NATURAL ℕ #-}

data List (A : Set) : Set where
  []  : List A
  _∷_ : (x : A) (xs : List A) → List A

append : {A : Set} -> List A -> List A -> List A
append [] ys = ys
append {A = X} (x ∷ xs) ys = x ∷ append {X} xs xs  -- NOTE: error here!

-- Hello World

_+_ : (n m : ℕ) → ℕ
zero  + m = m
suc n + m = suc (n + m)

module Explicit where

  data Vec (A : Set) : ℕ → Set where
    vnil  : Vec A zero
    vcons : (n : ℕ) → A → Vec A n → Vec A (suc n)

  vappend : {A : Set} (n m : ℕ) →
    Vec A n → Vec A m → Vec A (n + m)
  vappend .zero    m vnil           ys = ys
  vappend .(suc n) m (vcons n x xs) ys = vcons (n + m) x (vappend n m xs ys)


data Vec (A : Set) : ℕ → Set where
  []  : Vec A zero
  _∷_ : {n : ℕ} → A → Vec A n → Vec A (suc n)

vappend : {A : Set} {n m : ℕ} → Vec A n → Vec A m → Vec A (n + m)
vappend []       ys = ys
vappend (x ∷ xs) ys = x ∷ vappend xs ys

-- Basic propositions
------------------------------------------------------------------------

-- A proposition is the set of its proofs.

-- Conjunction

record _×_ (A B : Set) : Set where
  constructor _,_
  field fst : A
        snd : B

-- Implication

modusPonens : ∀{A B} → A × (A → B) → B
modusPonens (a , f) = f a

-- Disjunction

-- Either a b  in Haskell
data _⊎_ (A B : Set) : Set where
  left  : A → A ⊎ B
  right : B → A ⊎ B

-- A tautology involving disjunction

taut : ∀{A B} → (A → B) → A ⊎ B → B
taut f (left  a) = f a
taut f (right b) = b

-- Truth

record ⊤ : Set where

tt : ⊤
tt = record {}

-- Absurdity

data ⊥ : Set where

⊥-elim : {A : Set} → ⊥ → A
⊥-elim ()

¬ : (A : Set) → Set
¬ A = A → ⊥

-- Contraposition

contra : ∀{A B} → (A → B) → ¬ B → ¬ A
contra f g = λ x → g (f x)

-- Decidable propositions

data Dec (P : Set) : Set where
  yes : (p  :   P) → Dec P
  no  : (¬p : ¬ P) → Dec P

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
⇒≠base ()

⇒injl : ∀{a b c d} → (a ⇒ c) ≡ (b ⇒ d) → a ≡ b
⇒injl refl = refl

⇒injr : ∀{a b c d} → (a ⇒ c) ≡ (b ⇒ d) → c ≡ d
⇒injr refl = refl

-- Deciding type equality

eqTy : (a b : Ty) → Dec (a ≡ b)
eqTy base base = yes refl
eqTy base (b ⇒ b₁) = no λ()
eqTy (a ⇒ a₁) base = no λ()
eqTy (a ⇒ a₁) (b ⇒ b₁) with eqTy a b
eqTy (a ⇒ a₁) (b ⇒ b₁) | yes p with eqTy a₁ b₁
eqTy (a ⇒ a₁) (.a ⇒ .a₁) | yes refl | (yes refl) = yes refl
eqTy (a ⇒ a₁) (b ⇒ b₁) | yes p | (no ¬p) = no (λ x → ¬p (⇒injr x))
eqTy (a ⇒ a₁) (b ⇒ b₁) | no ¬p           = no (λ x → ¬p (⇒injl x))

-- Raw de Bruijn terms

data Exp : Set where
  var : (x   : ℕ  ) → Exp  -- de Bruijn index
  app : (f e : Exp) → Exp
  abs : (e   : Exp) → Exp

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
  vz : ∀{Γ a}                  → Var (a ∷ Γ) zero    a
  vs : ∀{Γ a b x} → Var Γ x a → Var (b ∷ Γ) (suc x) a

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
    neVar : ∀{b x}     → Var Γ x b                  → Ne Γ (var x) b
    neApp : ∀{a b f e} → Ne Γ f (a ⇒ b) → Nf Γ e a → Ne Γ (app f e) b

  data Nf (Γ : Cxt) : (e : Exp) (a : Ty) → Set where
    nfNe  : ∀{e b}   → Ne Γ e b       → Nf Γ e b
    nfAbs : ∀{a b e} → Nf (a ∷ Γ) e b → Nf Γ (abs e) (a ⇒ b)

example : ∀{A B} →
  Nf [] (abs (abs (app (var 0) (var 1)))) (A ⇒ ((A ⇒ B) ⇒ B))
example = nfAbs (nfAbs (nfNe (neApp (neVar vz) (nfNe (neVar (vs vz))))))

-- Type checking algorithm
------------------------------------------------------------------------

-- Sound context lookup

data Lookup (Γ : Cxt) (x : ℕ) : Set where
  yes : (a : Ty) (p : Var Γ x a) → Lookup Γ x
  no  : Lookup Γ x

lookupVar : ∀ Γ (x : ℕ) → Lookup Γ x
lookupVar [] x = no
lookupVar (a ∷ Γ) zero = yes a vz
lookupVar (a ∷ Γ) (suc x) with lookupVar Γ x
lookupVar (_ ∷ Γ) (suc x) | yes a p = yes a (vs p)
lookupVar (a ∷ Γ) (suc x) | no = no

-- Sound type checking

-- Result of type checking

data Infer (Γ : Cxt) (e : Exp) : Set where
  yes : (a : Ty) (p : Ne Γ e a) → Infer Γ e
  no  : Infer Γ e

data Check (Γ : Cxt) (e : Exp) (a : Ty) : Set where
  yes : (p : Nf Γ e a) → Check Γ e a
  no  : Check Γ e a

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

-- Sound type-checker (TODO: refactor into separate functions, one for each rule)

mutual
  check : ∀ Γ e a → Check Γ e a
  check Γ (var x) a with infer Γ (var x)
  check Γ (var x) a | yes b p with eqTy a b
  check Γ (var x) .b | yes b p₁ | (yes refl) = yes (nfNe p₁)
  check Γ (var x) a | yes b p | (no ¬p) = no
  check Γ (var x) a | no = no
  check Γ (app e e₁) a = {!!}  -- TODO: refactor the var case to not repeat the same thing
  check Γ (abs e) base = no
  check Γ (abs e) (a ⇒ b) with check (a ∷ Γ) e b
  check Γ (abs e) (a ⇒ b) | yes p = yes (nfAbs p)
  check Γ (abs e) (a ⇒ b) | no = no

  infer : ∀ Γ e → Infer Γ e
  infer Γ (var x) with lookupVar Γ x
  infer Γ (var x) | yes a p = yes a (neVar p)
  infer Γ (var x) | no = no
  infer Γ (app f e) with infer Γ f
  infer Γ (app f e) | yes base p = no
  infer Γ (app f e) | yes (a ⇒ b) p with check Γ e a
  infer Γ (app f e) | yes (a ⇒ b) p₁ | (yes p) = yes b (neApp p₁ p)
  infer Γ (app f e) | yes (a ⇒ b) p | no = no
  infer Γ (app f e) | no = no
  infer Γ (abs e) = no
