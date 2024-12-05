---
title: Interpreting functional languages
subtitle: Programming Language Technology, DAT151/DIT231
---


The lambda-calculus
-------------------

At the core of functional languages is the λ-calculus.
In pure λ-calculus, _everything is a function_.

Expressions `e` of pure λ-calculus are given by this grammar:
```haskell
    e,f ::= x        -- Variable
          | f e      -- Application of function f to argument e
          | λx → e   -- Function: abstraction of x in e
```
This is a subset of the Haskell expression syntax.
The abstract syntax corresponds to this LBNF grammar:
```lbnf
    EId.   Exp3 ::= Ident;
    EApp.  Exp2 ::= Exp2 Exp3;
    EAbs.  Exp  ::= "λ" Ident "→" Exp;

    coercions Exp 3;
```

Example:  `x y z`  should be read `(x y) z`.

### A small extension of the lambda-calculus

We consider an extension by `let`, numerals, and primitive operators:
```haskell
     ...  | let x = e in f
          | let x₁ = e₁; ...; xₙ = eₙ in f
          | n                   -- E.g. 0,1,2..
          | e₁ op e₂            -- op could be +,-,...
```
```lbnf
     ELet.  Exp  ::= "let" [Bind] "in" Exp;
     EInt.  Exp3 ::= Integer;
     EOp.   Exp1 ::= Exp1 Op Exp2;

     Bind.  Bind ::=  Ident "=" Exp;
     separator Bind ";";
```
`let x = e in f` could be regarded just syntactic sugar for `(λ x → f) e`,
and `let x₁ = e₁; ...; xₙ = eₙ in f` as sugar for
`(λ x₁ → ... λ xₙ → f) e₁ ... eₙ`.

But `let` is conceptually important.

Convenient syntactic sugar:

- multi-λ: `λ x₁ ... xₙ → e` sugar for `λ x₁ → .... → λ xₙ → e`
- let with arguments:
  `let f x₁ ... xₙ = e in ...` for `let f = λ x₁ ... xₙ → e in ...`

Running example:
```haskell
    let
      double x     = x + x
      comp   f g x = f (g x)
    in let
      twice  f     = comp f f
    in
        twice twice double 2
```
Example (Church numerals):
```haskell
    never  f = λ x → x
    once   f = λ x → f x
    twice  f = λ x → f (f x)
    thrice f = λ x → f (f (f x))
```

### Functional languages vs. imperative languages

In λ-calculus and functional languages, functions are _first class_.
They can be:

  1. called              (naturally, also in imperative languages)
  2. passed as arguments (in some imperative languages)
  3. returned as results (impossible in imperative languages)

E.g. 2. in C:

```c
int comp  (int f(int), int g(int), int x) {
  return f(g(x));
}
```

Trying 3. `let twice f = comp f f in twice double` in C:

- Attempt 1: illegal type
  ```c
  int twice (int f(int)) (int x) {
    return comp(f,f,x);
  }
  ... twice(double) ...
  ```

- Attempt 2: Cannot partially apply
  ```c
  int twice (int f(int), int x) {
    return comp(f,f,x);
  }
  ... twice(double,??) ...
  ```

The essential feature of functional languages is not anonymous
functions (via λ) but _partial application_, forming a new function by
giving less arguments than the function arity.

Example:  The increment function:
```haskell
    let plus x y = x + y in plus 1
```
Can be written as anonymous function `λ y → 1 + y`.

### Free and bound variables

In `λ x → x y`, variable `y` is considered _free_ while `x` is _bound_ by the `λ`.
In `let x = y in x`, variable `y` is free and `x` is bound by the `let`.
Both `let` and `λ` are _binders_.

Free variable computation:
```
    FV(x)                        = {x}
    FV(f e)                      = FV(f) ∪ FV(e)
    FV(λ x → e)                  = FV(e) \ {x}
    FV(let x₁=e₁;...;xₙ=eₙ in f) = FV(e₁,...,eₙ) ∪ (FV(f) \ {x₁,...,xₙ})
```
An expression without free variables is called _closed_, otherwise _open_.
For closed expression, we are interested in their _value_.

Running example continued:
  - `let ... in double 2` has value `4`
  - `let ... in twice double 2` has value `8`
  - `let ... in twice twice double 2` has value ??
  - `let ... in twice double` has value `λ x → e` where `e` computes `4*x`.
    (It has _normal form_ (maximally simplified form) `λ x → (x + x) + (x + x)`.)

A value `v` (in our language) can be a numeral (`int` value) or a λ (function value).

OBS! Some closed expressions do not have a value, e.g.

    (λ x → x x) (λ x → x x)

How to compute the value of an expression?

### Digression: Fix-point combinator and Russel paradox

Example (fix-point combinator)

    Y f = (λ x → f (x x)) (λ x → f (x x))

    Y f = f (Y f) = f (f (Y f)) = f (f (f (Y f))) ...

Example (faculty function)

    f n = if n <= 1 then 1 else n * f (n-1)

    Y f n = n!

Russel paradox: read application `x y` as `y ∈ x`.
Define the Russel set `x ∈ R` if `x ∉ x`:

    R x = not (x x)

Does `R` contain itself?

    R R = (λ x → not (x x)) (λ x → not (x x))
        = not (R R)
        = not (not (R R))
        = not (not (not ...))


Reduction
---------

Idea: compute the value by _substituting_ function arguments for
function parameters, and evaluating operations.
```
    (λ x → 1 + x) 2
    ↦  1 + 2
    ↦  3

    let x = 2 in 1 + x
    ↦  1 + 2
    ↦  3
```
This is formally a _small-step_ semantics `e ↦ e'`, called _reduction_.

Reduction rules:
```haskell
    (λ x → f) e               ↦  f[x=e]    -- named β by Alonzo Church
    let x = e in f            ↦  f[x=e]
    let x₁=e₁;...;xₙ=eₙ in f  ↦  f[x₁=e₁;...;xₙ=eₙ]
```
The lhs (left hand side) is called a _redex_ (reducible expression)
and the rhs (right hand side) its _reduct_.

_Strategy_ question: where and under which conditions can these rules be applied?

1. Full reduction: anywhere and unconditional
2. Leftmost-outermost reduction: Reduce the redex which is closest to
   the root of the expression tree, and if several exist, the leftmost
   of these.
3. Leftmost-innermost: Reduce the leftmost redex that does not contain any redexes in subexpressions.
4. Call-by-name: same as 2. but never reduce under λ.
5. Call-by-value: same as 3. but never reduce under λ, and thus do not consider anything under a λ as a redex.

Example:

    (λ y f → (λ x → x) f (f y))  ((λ z → z) 42)


### Substitution

Substitution `f[x=e]` of course does not substitute _bound_ occurrences of `x` in `f`:
```
      (λ x → (λ x → x)) 1
    ↦ (λ x → x)[x=1]
    = (λ x → x)
```
Still substitution has some pitfalls related to shadowing:

Example: `let x = 1 in (λ f x → f x) (λ y → x)`

- Reducing the `let` first:

          let x = 1 in (λ f x → f x) (λ y → x)
        ↦ ((λ f x → f x) (λ y → x))[x=1]
        = (λ f x → f x) (λ y → 1)
        ↦ (λ x → f x)[f = λ y → 1]
        = λ x → (λ y → 1) x
        ↦ λ x → 1[y=x]
        = λ x → 1

- Reducing the `λ` first:

          let x = 1 in (λ f x → f x) (λ y → x)
        ↦ let x = 1 in (λ x → f x)[f = λ y → x]
        = let x = 1 in λ x → (λ y → x) x
        ↦ let x = 1 in λ x → x[y=x]
        = let x = 1 in λ x → x
        ↦ (λ x → x)[x=1]
        = λ x → x

What goes wrong here?

Variable _capture_ problem in `(λ x → f x)[f = λ y → x]`:
Naive substituting under a binder can produce meaningless results.
Here, the free variable `x` is captured by the binder `λ x`.

Solutions:

1. Never substitute _open_ expressions under a binder.

   For evaluation of closed expressions, we can use strategies
   that respect this imperative.
   E.g., call-by-value, call-by-name.

2. Rename bound variables to avoid capture.

           (λ x → f x)[f = λ y → x]
         = (λ z → f z)[f = λ y → x]
         = (λ z → (λ y → x) z)
         ↦ λ z → x

Consistently renaming bound variables does not change the meaning of
an expression.

### Nontermination

There are terms that reduce to themselves!
```
      (λ x → x x) (λ x → x x)
    ↦ (x x)[x = (λ x → x x)]
    = (λ x → x x) (λ x → x x)
```
Such terms do not have a value.


Big-step semantics
------------------

We use the notation `let γ in e` where `γ` is a list of bindings `xᵢ = eᵢ`.
This list may be called _environment_.

Just like for C--, we can give a big step semantics `γ ⊢ e ⇓ v` for
the lambda-calculus.  In terms of reduction this should mean:

    (let γ in e) ↦* v

### Call-by-value

The big-step semantics corresponding to the call-by-value strategy
uses an environment of closed _values_, i.e., `γ` is of the form
`x₁=v₁,...,xₙ=vₙ`.

A value `v` is a closed expression which is
- a numeral (integer literal) `n`
- or a function with an environment `let δ in λ x → f`.

The latter is called a
_closure_ and may be written `⟨λx→f; δ⟩` or `(λx→f){δ}` (IPL book).

#### Evaluation rules:

Variable:
```
     ------------
     γ ⊢ x ⇓ γ(x)
```
Let:
```
     γ      ⊢ e₁ ⇓ v₁
     γ,x=v₁ ⊢ e₂ ⇓ v₂
     -------------------------
     γ ⊢ let x = e₁ in e₂ ⇓ v₂
```
Lambda:
```
     --------------------------------
     γ ⊢ (λx → f) ⇓ (let γ in λx → f)
```
Application:
```
     γ      ⊢ e₁ ⇓ let δ in λx → f
     γ      ⊢ e₂ ⇓ v₂
     δ,x=v₂ ⊢ f ⇓ v
     -----------------------------
     γ ⊢ e₁ e₂ ⇓ v
```
Exercise:  Evaluate `(((λ x₁ → λ x₂ → λ x₃ → x₁ + x₃) 1) 2) 3`

Rules for integer expressions:
```
     ---------
     γ ⊢ n ⇓ n

     γ ⊢ e₁ ⇓ n₁
     γ ⊢ e₂ ⇓ n₂
     ---------------- n = n₁ + n₂
     γ ⊢ e₁ + e₂ ⇓ n
```

Drawback of call-by-value: unused arguments are still evalutated.

Example:

    (\ y -> 1) (twice twice double 2)

The result is `1`, but call-by-value will first compute the value of `twice twice double 2`.

### Call-by-name

Idea: only evaluate expressions when needed.

Call by name differs from call-by-value by not evaluating arguments
when calling functions, but to form a closure.  An environment entry
`c` is now itself a closure `let δ in e` where environment `δ` is of
the form `x₁=c₁,...,xₙ=cₙ`.

The evaluation judgement is still of the form `γ ⊢ e ⇓ v`.

#### Evaluation rules:

Variable:
```
     δ ⊢ e ⇓ v
     ---------- γ(x) = let δ in e
     γ ⊢ x ⇓ v
```
Let:
```
     γ,x=c ⊢ e₂ ⇓ v₂
     ------------------------- c = let γ in e₁
     γ ⊢ let x = e₁ in e₂ ⇓ v₂
```
Lambda (unchanged):
```
     --------------------------------
     γ ⊢ (λx → f) ⇓ (let γ in λx → f)
```
Application:
```
     γ     ⊢ e₁ ⇓ let δ in λx → f
     δ,x=c ⊢ f ⇓ v
     ----------------------------- c = let γ in e₂
     γ     ⊢ e₁ e₂ ⇓ v
```
Rules for integer expressions: (unchanged).


Comparing cbn (call-by-name) with cbv (call-by-value):

- Arguments are _only_ evaluated when _needed_.
  This is an advantage if an argument is unused or only used under rare conditions.
  E.g.:
  `e₁ && e₂ = if e₁ then e₂ else false`

- Arguments are _every time_ evaluated when _needed_.
  This is a disadvantage if an argument is used twice or more.
  E.g.:
  `double x = x + x`


### Call-by-need

Synthesis of call-by-name and call-by-value: _call-by-need_ (Haskell)

- _lazy_: only evaluate when needed, but then store value
- next time the value is needed, grab stored value
- needs global heap rather than local environment

Call-by-need example:
```haskell
    twice twice double 2

    double x = add x x
    comp   f = \ g x -> f (g x)
    twice  f = comp f f
```

Step visualization with https://github.com/well-typed/visualize-cbn:

<div style="margin-left: 50px;">
<a onclick="cbnPrev()"><button>Previous</button></a>
<a onclick="cbnNext()"><button>Next</button></a>
[step <span id="cbn_step">Step</span>, <span id="cbn_status">Status</span>]

<table width="100%" border="0" cellpadding="0">
<!-- style="border-collapse: collapse;"> -->
<tr><td><div style="font-family: monospace;" id="cbn_term">Term</div></td></tr>
<!--  border-bottom: 1px solid black; -->
<tr><td><div style="font-family: monospace;" id="cbn_heap">Heap</div></td></tr>
</table>

<script type="text/javascript" src="twice-double.js"></script>
</div>

Literature to study call-by-need:

- big-step semantics:

  John Launchbury:
  A Natural Semantics for Lazy Evaluation.
  POPL 1993: 144-154

- small-step semantics:

  Peter Sestoft:
  Deriving a Lazy Abstract Machine.
  J. Funct. Program. 7(3): 231-264 (1997)
