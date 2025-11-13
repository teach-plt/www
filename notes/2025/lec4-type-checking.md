2025-11-13 Andreas Abel, transcript of lecture by Magnus Myreen

Lecture 4: Type Checking
========================

IDEs complain about parts of programs, e.g.:
```
       z = x / y
          ~~~
```

Why is type checking important?

- Compiler might not know what to do, e.g. integer division vs. floating point division.
- Type checking proves certain properties of your program.
- Type soundness in strongly-typed programming languages:

        typeable p  ==>  semantics p ≠ crash


**Specification**:

A type system is defined by _inference rules_ defining a relation that is a _type judgement_.

        Γ ⊢ e : t

**Implementation**:

- checker
- inference

Inference rules
---------------

Format:

        J₀ J₁ ... Jₙ
        ------------ C
             J

### Example: even numbers

                   even n
        ------     ------------
        even 0     even (n + 2)

Claim: `even 6`.  Has to be proven with the rules.

        ------
        even 0
        ------
        even 2
        ------
        even 4
        ------
        even 6

More rules, e.g.

        even n      even m
        ------------------
        even (n + m)

Allows more proofs

        ------    ------
        even 0    even 0
        ------    ------
        even 2    even 2
        ----------------
             even 4

### Example

                  n+1 ↝ m
        ------    -------
        n ↝ n     n ↝ m

We can derive `1 ↝ 3` but not `4 ↝ 3`.

        -----
        3 ↝ 3
        -----
        2 ↝ 3
        -----
        1 ↝ 3

So `n ↝ m` is `n ≤ m`.


Typing judgement
----------------

Example: rules for divison

        e₁ : int      e₂ : int
        ----------------------
        e₁ / e₂ : int

        e₁ : double   e₂ : double
        -------------------------
        e₁ / e₂ : double

Generically:

        e₁ : t      e₂ : t
        ------------------ t ∈ {int, double}
        e₁ / e₂ : t

Writing it properly with ASTs:

        e₁ : t      e₂ : t
        ------------------ t ∈ {int, double}
        EDiv e₁ e₂ : t

E.g. `2 / 3` is abstract syntax tree `EDiv (ELitInt 2) (ELitInt 3)`.

        ---------------
        ELitInt i : int

With concrete syntax, this gets a bit ambiguous:

        -------
        i : int

Needs a side condition that `i` is an integer literal.
Convention:

- `i` stands for integer literals
- `e` stands for expressions

Checking and inference
----------------------

Checking: given an expression `e` and a type `t`, answer yes (`e : t`) or no.

        check : (Exp, Typ) → Bool

Inference: given an expression `e`, produce its type `t` if it has one.

        infer : Exp → Maybe Typ

What is the connection to the typing rules?
Write some pseudo-code...

        check (ELitInt i, t):  return (t == int)

        infer (ELitInt x):     return (Just int)

        check (EDiv e₁ e₂, t): return check (e₁, t) && check (e₂, t) && t ∈ {int, double}

        infer (EDic e₁, e₂):
          t₁ ← infer e₁
          t₂ ← infer e₂
          assert (t₁ == t₂)
          assert (t₁ ∈ {int,double})
          return t₁

Annotating type checker
-----------------------

Type checker transforms

        EDiv (EVar "y") (EVar "z")

to type-annotated expressions

        EDivT int (EVarT "y" int) (EVarT "z" int)

Subtyping
---------

Example:

        int k = 5;
        double d = k / 2.0;

This would be rejected by the type checker.

New rule:

        e : t₁
        ------  t₁ ≤ t₂
        e : t₂

Subtyping `t₁ ≤ t₂`, e.g. `int ≤ double`.
Derivation:

        k : int
        ----------     ------------
        k : double     2.0 : double
        ---------------------------
        k / 2.0 : double

It is not _syntactically_ clear when we should flip the type from `int` to `double`.
The rule allows it at any point.

The type annotated expressions need to retain evidence of the use of the subtyping rule.

         ECoerceT int double e

Booleans
--------

        e₁ : t    e₂ : t
        ---------------- t ∈ {int, double, bool}
        e₁ == e₂ : bool

Big picture
-----------

- check whole program
- we started on expressions, we also need statements

So what is the type of assignment statement, e.g. `x = 5;`?

For statements we write `⊢ s` to mean statement `s` is type-correct.

How to we indicate that something has a specific return type?

E.g.

         if (...) { return 5; } else { return 2.5; }

Type correct:

         |-^double  if (...) { return 5; } else { return 2.5; }

Not correct:

         |-^int     if (...) { return 5; } else { return 2.5; }

Rule:

         e : t
         ------------
         ⊢ᵗ return e;

We are still missing the typing of variables.
This is where the environment `Γ` comes into play.

Example program:

        int f (double x) {
          int i = 2;
          int j = 5;
          // int i = 3;  // this would not be allowed, since i is already declared in this block
          {
            int x = 0;    // ok to overwrite x because we are in a different scope
            double i = 3; // ok to overwrite i because we are in a different scope
            {
                ... i + x ...  // these refer to i and x of the inner scope
                               // Context here:
                               // (x:double, i:int, j:int), (x:int, i:double)
                               // consists of 2 blocks
            }
            int k;
          }
          i++;
        }

Rules with context:

        lookup x in Γ gives t
        ---------------------
        Γ ⊢ x : t

Example:

        Γ(x) = int
        -----------                -----------
        Γ ⊢ x : int                Γ ⊢ 1 : int
        --------------------------------------
        Γ ⊢ x + 1 : int

Judgements with context
-----------------------

        Γ ⊢ e : t      -- Γ context, e expression, t type

        Γ ⊢ᵗ s ⇒ Γ'     -- Γ initial context, s statement, Γ' post-context, t return type

Example: rule for variable declaration

        -------------------
        Γ ⊢ int i ⇒ Γ,i:int

Sequence

        Γ ⊢ s₁ ⇒ Γ'    Γ' ⊢ s₂ ⇒ Γ''
        -----------------------------
        Γ ⊢ s₁ s₂ ⇒ Γ''

Assignment statement

        x:t ∈ Γ      Γ ⊢ e : t
        ----------------------
        Γ ⊢ x = e; ⇒ Γ

If-statement

        Γ ⊢ e : bool   Γ ⊢ s₁ ⇒ Γ₁    Γ ⊢ s₂ ⇒ Γ₂
        ------------------------------------------
        Γ ⊢ if (e) s₁ else s₂ ⇒ Γ

While-statement

        Γ ⊢ e : bool   Γ ⊢ s ⇒ Γ₁
        -------------------------
        Γ ⊢ while (e) s ⇒ Γ

Function calls

        Σ; Γ ⊢ eᵢ : tᵢ  (i=1..n)
        ------------------------------ f : (t₁, t₂, ..., tₙ) → t ∈ Σ
        Σ; Γ ⊢ f (e₁, e₂, ..., eₙ) : t

Σ is the _function context_ mapping function names `f` to function types `(t₁, t₂, ..., tₙ) → t`.

Big picture completed
---------------------

To check a program consisting of function definitions:

1. Construct function context `Σ`
2. Check all function bodies

             Σ; Γ ⊢ body ⇒ Γ'

   where `Γ` contains the formal parameters of the function
   (and `Γ'` does not matter).
