Typing the lambda calculus
==========================

Lambda calculus with integers:

    e,f ::= x                 -- variable
          | λx → e            -- function abstraction
          | f e               -- application
          | let x = e₁ in e₂  -- local binding
          | n                 -- integer literal
          | e₁ op e₂          -- arithmetic operation

Simple types
------------

Grammar for types:

    t,s ::= int         -- base type of integers
          | s → t       -- function type

Arrow associates to the _right_, so

    r → s → t = r → (s → t)

Higher order functions:

    twice : (int → int) → (int → int)
    twice = λ f → λ x → f (f x)

Order of a function type:

    ord(int)   = 0
    ord(s → t) = max { ord(s) + 1, ord(t) }

Examples:

  - 1st order function:  Takes base values as inputs

        int → int
        int → int → int

        ord(int → (int → int))
          = max { ord(int)+1, ord(int → int) }
          = max { ord(int)+1, ord(int)+1, ord(int) }
          = max {1,1,0} = 1

  - 2nd order function: Accepts even 1st order functions

        (int → int) → int
        (int → int) → int → int
        (int → int) → (int → int) → int
        (int → int → int) → int → int

  - 3rd order function: Has a 2nd order function as argument

        ((int → int) → int) → int


Typing rules
------------

Judgement:  `Γ ⊢ e : t`.

Context Γ maps variables to types.

- Variable:

        --------- Γ(x) = t
        Γ ⊢ x : t

- Abstraction:

        Γ,x:s ⊢ e : t
        -----------------------
        Γ ⊢ (λ x → e) : (s → t)

- Application:

        Γ ⊢ f : s → t
        Γ ⊢ e : s
        -------------
        Γ ⊢ f e : t

  Alternatively:

        Γ ⊢ f : s₁ → t
        Γ ⊢ e : s₂
        -------------- s₁ = s₂
        Γ ⊢ f e : t

- Let:

        Γ ⊢ e₁ : s
        Γ,x:s ⊢ e₂ : t
        --------------------------
        Γ ⊢ (let x = e₁ in e₂) : t

Example:  Deriving `⊢ λ f → λ x → f (f x) : (int → int) → int → int`

                                                 -------------------  -------------
                                                 ... ⊢ f : int → int  ... ⊢ x : int
    --------------------------------------       ----------------------------------
    f : int → int, x : int ⊢ f : int → int       f : int → int, x : int ⊢ f x : int
    -------------------------------------------------------------------------------
    f : int → int, x : int ⊢ f (f x) : int
    -------------------------------------------
    f : int → int  ⊢  λ x → f (f x) : int → int
    -----------------------------------------------
    ⊢ λ f → λ x → f (f x) : (int → int) → int → int


### Difficulties with implementing the typing rules

Naive type checking gets stuck at application.

    void check(Cxt Γ, Exp e, Type t)

    check(Γ, λx→e, int): type error

    check(Γ, λx→e, s → t):
      check (Γ[x:s], e, t)

    check(Γ, f e, t):
      let s = ???
      check(Γ, f, s→t)
      check(Γ, e, s)

Naive inference gets stuck at abstraction.

    Type infer(Cxt Γ, Exp e)

    infer(Γ, f e):
      case infer(Γ,f) of
        int:     type error
        (s → t): if s == infer(Γ,e)
                   then return t
                   else type error

    infer(Γ, λx→e):
      let s = ???
      let t = infer(Γ[x:s], e)
      return (s → t)

Solutions:

  1. Bidirectional type-checking:
     * two modes: checking and inference
     * check λs,
     * infer applications: infer function part, check argument part
     * `let x = e₁ in e₂`:
        - `e₁` needs to be inferred
        - `e₂` can be checked or inferred
     + simple, easy to understand, good error messages
     - cannot handle β-redexes in general: `(λ x → f) e`
     - in practice, too weak

  2. Typed λ  `λ(x:s) → e` and let `let x:s = e₁ in e₂`
     * Allows inferring λs and lets, e.g.

        infer(Γ, λ(x:s)→e):
          let t = infer(Γ[x:s], e)
          return (s → t)

     + simple, good error messages
     - requires a lot of redundant type annotations from the user (like in Java)

         C x = new C(e);
         let x:int = 3 in x + x


  3. Inference with type variables
     * Treat the `???`s as variables for types
     * Collect constraints on the type variables
     * Solve constraints by _unification_

Example:  Inferring the `comp` function.

    ⊢ λ f → λ g → λ x → f (g x) : A
    A = B → C
    f:B ⊢  λ g → λ x → f (g x) : C
    C = D → E
    f:B, g:D ⊢  λ x → f (g x) : E
    E = F → G
    f:B, g:D, x:F ⊢ f (g x) : G
    - f:B, g:D, x:F ⊢ f : H → G
      B = H → G
    - f:B, g:D, x:F ⊢ g x : H
      * f:B, g:D, x:F ⊢ g : I → H
        D = I → H
      * f:B, g:D, x:F ⊢ x : I
        F = I

Constraints (solved constraints above the bar):

    A = B → C
    C = D → E
    E = F → G
    B = H → G
    D = I → H
    F = I

    A = B → C
    ---------
    C = D → E
    E = F → G
    B = H → G
    D = I → H
    F = I

    A = B → (D → E)
    C = D → E
    ---------
    E = F → G
    B = H → G
    D = I → H
    F = I

    A = B → (D → (F → G))
    C = D → (F → G)
    E = F → G
    ---------
    B = H → G
    D = I → H
    F = I

    A = (H → G) → (D → (F → G))
    C = D → (F → G)
    E = F → G
    B = H → G
    ---------
    D = I → H
    F = I

    A = (H → G) → ((I → H) → (F → G))
    C = (I → H) → (F → G)
    E = F → G
    B = H → G
    D = I → H
    ---------
    F = I

    A = (H → G) → ((I → H) → (I → G))
    C = (I → H) → (I → G)
    E = I → G
    B = H → G
    D = I → H
    F = I
    ---------

    ⊢ λ f → λ g → λ x → f (g x) : (H → G) → ((I → H) → (I → G))
    ⊢ λ f → λ g → λ x → f (g x) : ∀ α β γ. (β → γ) → ((α → β) → (α → γ))


Type inference with type variables and unification
--------------------------------------------------

Extended type grammar:

    s,t ::= ...
          | X     -- type variable

A type that does not mention any type variable is called _closed_.

A _substitution_ `σ` maps type variables `X` to types `t`.
The application of a substitution `σ` to a type `t` is written `tσ`.

     X       σ = σ(X)
     int     σ = int
     (s → t) σ = sσ → tσ

A substitution `σ` is usually given by a finite list of mappings `X₁=s₁,...Xₙ=sₙ`.
Then `σ(Xᵢ) = sᵢ` and `σ(Y)=Y` if `Y ∉ {X₁,...Xₙ}`.

Example:

     (X → U)[X = Y → int] = (Y → int) → U

The application of a substitution `σ` to another substitution `τ` is written `τσ`.
This is also called the _composition_ of substitutions and written `σ ∘ τ` (see book).

     t(τσ) = (tτ)σ

So:

     (τσ)(X) = (τ(X))σ

Example for substitution composition:

     [X=Y→int][Y=int] = [X=int→int,Y=int]

        X[X=Y→int][Y=int] = (Y→int)[Y=int] = int → int
        Y[X=Y→int][Y=int] = Y[Y=int] = int
        Z[X=Y→int][Y=int] = Z[Y=int] = Z


State of the type inference:

1. Potentially infinite supply of type variables (e.g. X₀, X₁, ...)
    Allows allocation of new type variable

        Type newTypeVariable

2. Store for equality constraints.
   A constraint `s ≐ t` is a pair of types `s` and `t` that need to be unified.

        void equal (Type s, Type t)

   adds the new constraint `s ≐ t` to the store.


Inference phase:  Build up store of constraints.

    infer(Γ, x):
      return lookup(Γ,x)

    infer(Γ, λx→e):
      s ← newTypeVariable
      t ← infer(Γ[x:s], e)
      return (s → t)

    infer(Γ, f e):
      r ← infer(Γ, f)
      case r of
        int    : type error
        (s → t):
           s' ← infer(Γ, e)
           equal(s,s')
           return t
        X: s ← infer(Γ, e)
           t ← newTypeVariable
           equal(X,s→t)
           return t

Simplification of application case:

    infer(Γ, f e):
      r ← infer(Γ, f)
      s ← infer(Γ, e)
      t ← newTypeVariable
      equal(r,s→t)
      return t


Solution phase:
Try to find a substitution σ from type variables to types that solves all constraints.

State of solver:

1. Constraint store:  Additional methods:

         Bool solved()                -- is the store empty?
         Constraint takeConstraint()  -- extract a constraint, removing it from the store

2. A substitution.
   Invariant:  The substitution is already applied to the state (constaints and itself).

         void solveVar(TypeVariable X, Type t)

   `solveVar` applies substitution `[X=t]` to the state
   (all constraints and the substitution)

Algorithm:

    while (not solved()) {
      (s ≐ t) ← takeConstraint()
      unify(s,t)
    }

Unification:

    unify(X,X)
    unify(int, int):
       -- do nothing

    unify(s₁→t₁, s₂→t₂):
      equal(s₁,s₂)
      equal(t₁,t₂)

    unify(int,s→t)
    unify(s→t,int):
      type error

    unify(X,t)
    unify(t,X):
      if X occurs in t then type error
      else solveVar(X,t)


Example:  Compute type of `twice`

  - Inference phase

        infer(ε, λ f → λ x → f (f x)) = F → X → Z
        infer(f:F, λ x → f (f x)) = X → Z
        infer(f:F,x:X, f (f x)) = Z
        - infer(...,f) = F
        - infer(...,f x) = Y
          * infer (..., f) = F
          * infer (..., x) = X
          * F = X → Y
          * result: Y
        - F = Y → Z
        - result Z

  - Solving phase (substitution on top, constraints on bottom)

        -------------
        F = X → Y
        F = Y → Z

        F = X → Y
        -------------
        X → Y = Y → Z

        F = X → Y
        -------------
        X = Y
        Y = Z

        F = Y → Y
        X = Y
        -------------
        Y = Z

    Final substitution:

        F = Z → Z
        X = Z
        Y = Z
        --------------

  - Applied to computed type `F → X → Z` gives `(Z → Z) → Z → Z`.


Example:  Compute type of `λ x → x x`

    infer(ε, λ x → x x)
    infer(x:X, x x)
    X = X → Y
    result: Y


Polymorphism
------------

Some variables may be left unconstrained, e.g.:

    comp : (Y → Z) → (X → Y) → (X → Z)

`comp` should have any _instance_ of this type:
Any instantiation of `X`, `Y`, `Z` gives a valid type.
`comp` has polymorphic type:

    comp : ∀ α β γ.  (β → γ) → (α → β) → α → γ

Extension of type-inference (sketch):

  - Extend the grammar by _universal_ variables `α`
    (`X` are _existential_ variables).

  - A type _scheme_ is of the form `∀ α₁...αₙ. t`

  - Context `Γ` maps variables `x` to schemes.

  - When we lookup a scheme in the context, `Γ(x)`,
    instantiate the scheme to fresh existential variables `X₁...Xₙ`.

  - When the type `t` of expression `e₁` in `let x = e₁ in e₂`
    has unconstrained existential variables `X₁...Xₙ`
    that do not appear in the current context `Γ`, replace them
    by universal variables `α₁...αₙ` and assign `x` the scheme
    `∀α₁...αₙ.t[α₁...αₙ/X₁...Xₙ]`.
