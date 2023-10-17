-- Programming Language Technology (Chalmers DAT 151, GU DIT 231)
-- (C) Andreas Abel, 2023
-- All rights reserved.

-- Imports from the standard library and additional Haskell bindings.
------------------------------------------------------------------------

module Library where

------------------------------------------------------------------------
-- Unqualified imports
------------------------------------------------------------------------

-- Floats
------------------------------------------------------------------------

open import Agda.Builtin.Float public using (Float) renaming
  ( primFloatEquality to _==?ᵈ_
  ; primFloatLess     to _<?ᵈ_
  ; primNatToFloat    to ℕ→double
  ; primFloatPlus     to _+ᵈ_
  ; primFloatMinus    to _-ᵈ_
  ; primFloatTimes    to _*ᵈ_
  ; primFloatDiv      to _/ᵈ_
  )
1ᵈ = ℕ→double 1

-- Base modules
------------------------------------------------------------------------

open import Data.Bool.Base     public using (Bool; true; false; _xor_; not; if_then_else_) hiding (module Bool)
open import Data.Char.Base     public using (Char)
open import Data.Empty         public using (⊥)
open import Data.Integer.Base  public using (ℤ; -[1+_]; +_)
open import Data.List.Base     public using (List; []; _∷_; [_]; _++_) hiding (module List)
open import Data.List.NonEmpty public using (List⁺; _∷_; _∷⁺_) hiding (module List⁺)

open import Data.Maybe.Base    public using (Maybe; nothing; just)
open import Data.Nat.Base      public using (ℕ; zero; suc; _+_; _≤_; s≤s) hiding (module ℕ)
open import Data.Product       public using (∃; ∃₂; _×_; _,_; proj₁; proj₂; map₂; uncurry)
  renaming (map to ∃-map)
open import Data.String.Base   public using (String) renaming (_++_ to _<>_)
open import Data.Sum.Base      public using (_⊎_; inj₁; inj₂)
open import Data.Unit.Base     public using (⊤)

open import Function           public using (id; _∘_; _∘′_; _$_; case_of_)
open import Level              public using (Level; _⊔_)

open import IO.Primitive       public using (IO)

open import Relation.Binary    public using (Decidable; Rel)
open import Relation.Nullary   public using (¬_; Dec; yes; no)

-- Advanced modules (long names)
------------------------------------------------------------------------

open import Relation.Binary.PropositionalEquality public using (_≗_; _≡_; refl; trans; cong; subst)
open import Relation.Nullary.Decidable            public using (⌊_⌋) renaming (map′ to mapDec)

------------------------------------------------------------------------
-- Qualified imports
------------------------------------------------------------------------

-- Booleans
------------------------------------------------------------------------

module Bool where
  open import Data.Bool public using (_≟_; _∧_)

  _==_ : (b b' : Bool) → Bool
  b == b' = ⌊ b ≟ b' ⌋

-- Natural numbers
------------------------------------------------------------------------

module ℕ where
  open import Data.Nat.Base public
  open import Data.Nat.Properties public

-- Integers
------------------------------------------------------------------------

module Integer where
  open import Data.Integer public

  infix 1 _==_ _<=_

  _==_ : (i j : ℤ) → Bool
  i == j = ⌊ i ≟ j ⌋

  _<=_ : (i j : ℤ) → Bool
  i <= j = ⌊ i ≤? j ⌋

  postulate div : (i j : ℤ) → ℤ
  {-# COMPILE GHC div = div #-}

-- Lists
------------------------------------------------------------------------

module List where
  open import Data.List.Base public
    using ([_]; _++_; concat; map; foldl; foldr; reverse; sum; fromMaybe; intersperse)

  infixl 5 _++ʳ_

  -- Reverse append.  (Soon in the std-lib as _ʳ++_.)

  _++ʳ_ : ∀ {a} {A : Set a} → List A → List A → List A
  xs ++ʳ ys = foldl (λ rev x → x ∷ rev) ys xs

-- Non-empty lists
------------------------------------------------------------------------

module List⁺ where
  open import Data.List.NonEmpty public using (toList; tail)

-- Strings
------------------------------------------------------------------------

module String where
  open import Data.String.Base public

-- Pretty printing
------------------------------------------------------------------------

parens : String → String
parens s = "(" <> s <> ")"

infixl 6 _<+>_

_<+>_ : String → String → String
s <+> s' = s <> " " <> s'

vcat : List (List String) → List String
vcat = List.concat

vsep : List (List String) → List String
vsep = List.concat ∘ List.intersperse [ "" ]

-- Error monad
------------------------------------------------------------------------

module ErrorMonad {e} {E : Set e} where

  Error : ∀{a} (A : Set a) → Set (e ⊔ a)
  Error A = E ⊎ A

  pattern fail err = inj₁ err
  pattern ok   val = inj₂ val

  return : ∀{a}{A : Set a} → A → Error A
  return = ok

  _>>=_ : ∀{a b} {A : Set a} {B : Set b} → Error A → (A → Error B) → Error B
  fail err >>= k = fail err
  ok   a   >>= k = k a

  _>>_ : ∀{b} {B : Set b} → Error ⊤ → Error B → Error B
  m >> m' = m >>= λ _ → m'

  _<&>_ : ∀{a b} {A : Set a} {B : Set b} → Error A → (A → B) → Error B
  fail e <&> f = fail e
  ok   a <&> f = ok (f a)

  _<$>_ : ∀{a b} {A : Set a} {B : Set b} → (A → B) → Error A → Error B
  f <$> m = m <&> f

  liftM2 : ∀ {a b c} {A : Set a} {B : Set b} {C : Set c}
    (f : A → B → C) → Error A → Error B → Error C
  liftM2 f m n = do
    a ← m
    f a <$> n

  throwError : ∀{a} {A : Set a} → E → Error A
  throwError = fail

  catchError : ∀{a} {A : Set a} → Error A → (E → Error A) → Error A
  catchError (fail e) h = h e
  catchError (ok a)   h = ok a

-- IO as Functor, Applicative, Monad
------------------------------------------------------------------------

open import Category.Functor     using (RawFunctor)
open import Category.Applicative using (RawApplicative)

module IOFunctor where
  open import IO.Primitive public using (return; _>>=_)

  infixl 1 _>>_

  _>>_  : ∀ {b} {B : Set b} → IO ⊤ → IO B → IO B
  _>>_ = λ m m' → m >>= λ _ → m'

  infixr 1 _=<<_

  _=<<_  : ∀ {a b} {A : Set a} {B : Set b} → (A → IO B) → IO A → IO B
  k =<< m = m >>= k

  -- The following definition is needed to create the RawFunctor instance.

  infixl 4 _<$>_

  _<$>_ :  ∀ {a b} {A : Set a} {B : Set b} → (A → B) → IO A → IO B
  f <$> m = m >>= λ a -> return (f a)

-- This module is needed to create the RawApplicative instance
-- in a way compatible with both std-lib v1.7.1/2 and v2.0.

module IOApplicative where
  open IOFunctor public

  rawFunctor : ∀{ℓ} → RawFunctor (IO {a = ℓ})
  rawFunctor = record { IOFunctor }

  pure : ∀{a} {A : Set a} → A → IO A
  pure = return

  infixl 4 _<*>_ _⊛_

  _<*>_ : ∀ {a b} {A : Set a} {B : Set b} → IO (A → B) → IO A → IO B
  mf <*> ma = mf >>= λ f → ma >>= λ a → return (f a)

  _⊛_ : ∀ {a b} {A : Set a} {B : Set b} → IO (A → B) → IO A → IO B
  _⊛_ = _<*>_

module IOMonad where
  open IOApplicative public

  -- Field rawApplicative is part of the RawMonad of std-lib v2.0
  -- Thus we have to construct the Applicative implementation
  -- even though we do not use it.
  --
  rawApplicative : ∀{ℓ} → RawApplicative (IO {a = ℓ})
  rawApplicative = record { IOApplicative }

------------------------------------------------------------------------
-- Haskell bindings
------------------------------------------------------------------------

{-# FOREIGN GHC import qualified Data.List #-}
{-# FOREIGN GHC import qualified Data.Text #-}
{-# FOREIGN GHC import qualified Data.Text.IO #-}
{-# FOREIGN GHC import qualified System.Exit #-}
{-# FOREIGN GHC import qualified System.Environment #-}
{-# FOREIGN GHC import qualified System.FilePath #-}
{-# FOREIGN GHC import qualified System.IO #-}
{-# FOREIGN GHC import qualified System.Process #-}

-- Binding more Haskell functions

postulate
  exitFailure    : ∀{a} {A : Set a} → IO A
  getArgs        : IO (List String)
  putStrLn       : String → IO ⊤
  readFiniteFile : String → IO String
  readInt        : IO ℤ
  readDouble     : IO Float
  takeBaseName   : String → String
  takeDirectory  : String → String
  writeFile      : String → String → IO ⊤
  callProcess    : String → List String → IO ⊤

{-# COMPILE GHC exitFailure    = \ _ _ -> System.Exit.exitFailure #-}
{-# COMPILE GHC getArgs        = map Data.Text.pack <$> System.Environment.getArgs #-}
{-# COMPILE GHC putStrLn       = System.IO.putStrLn . Data.Text.unpack #-}
{-# COMPILE GHC readFiniteFile = Data.Text.IO.readFile . Data.Text.unpack #-}
{-# COMPILE GHC readInt        = (System.IO.readLn :: System.IO.IO Integer) #-}
{-# COMPILE GHC readDouble     = (System.IO.readLn :: System.IO.IO Double)  #-}
{-# COMPILE GHC takeBaseName   = Data.Text.pack . System.FilePath.takeBaseName . Data.Text.unpack #-}
{-# COMPILE GHC takeDirectory  = Data.Text.pack . System.FilePath.takeDirectory . Data.Text.unpack #-}
{-# COMPILE GHC writeFile      = Data.Text.IO.writeFile . Data.Text.unpack #-}
{-# COMPILE GHC callProcess    = \ cmd -> System.Process.callProcess (Data.Text.unpack cmd) . Data.List.map Data.Text.unpack #-}

-- Showing builtin types
------------------------------------------------------------------------

postulate
  printNat : ℕ → String
  printInt : ℤ → String
  printDouble : Float → String

{-# COMPILE GHC printNat    = \ i -> Data.Text.pack (show (i :: Integer)) #-}
{-# COMPILE GHC printInt    = \ i -> Data.Text.pack (show (i :: Integer)) #-}
{-# COMPILE GHC printDouble = \ d -> Data.Text.pack (show (d :: Double )) #-}

printBool : Bool → String
printBool true  = "true"
printBool false = "false"
