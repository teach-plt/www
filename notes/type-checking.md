Programming Language Technology
DAT151/DIT231
Andreas Abel

Type Checking
=============

Introduction
------------

### Purposes of types

- Machine perspective:
  * Numeric types and arithmetic: 32/64bit word, signed/unsigned, float/integer
    ```c
    z = x / y;
    ```
  * Avoid runtime errors (e.g. by confusion of number and memory address)
    ```c
    typedef int (*fun_t)();  // Pointer to function without arguments returning int

    int main () {
      fun_t good = &main;    // Function pointer to main function
      int i = (*good)();
      fun_t bad  = 12345678; // Function pointer to random address
      int j = (*bad)();
    }
    ```

- Human perspective:
  * Avoid silly mistakes (types provide some redundancy)
    ```c
    void foo (int x) { ... }
    ...
    int   piece;
    char* peace;
    ...
    foo (peace);
    ```
  * Better code comprehension (documentation)


### Where types make a difference

```c
... divide(... x, ... y) { return x / y; }

printDouble (divide (5, 3));
```

Judgements and rules
--------------------

Rule format:

    J₁ ... Jₙ
    --------- C
        J

- J : conclusion
- Jᵢ: premises/hypotheses
- C : side condition

Logical interpretation: Judgement J holds if C and all of J₁ ... Jₙ hold.

Algorithmic interpretation: To check J, check J₁ ... Jₙ (in this order)
and C (in a suitable place).


Typing of arithmetic expressions
--------------------------------

Judgement version 1: "expression `e` has type `t`"

    e : t


### Concrete or abstract syntax

Rules may be written using concrete syntax:

    e₁ : int    e₂ : int
    --------------------
    e₁ / e₂ : int

    e₁ : double    e₂ : double
    --------------------------
    e₁ / e₂ : double

Generic version, using side condition:

    e₁ : t    e₂ : t
    ---------------- t ∈ {int, double}
    e₁ / e₂ : t

Using abstract syntax:
```lbnf
TInt.    Type ::= "int";
TDouble. Type ::= "double";

EInt.     Exp6 ::= Integer;
EDouble.  Exp6 ::= Double;
EDiv.     Exp2 ::= Exp3 "/" Exp2;
```

    -------------        -------------------
    EInt i : TInt        EDouble d : TDouble

    e₁ : t    e₂ : t
    ---------------- t ∈ {TInt, TDouble}
    EDiv e₁ e₂ : t

### Type checking and type inference

Checking:  Given `e` and `t`, check whether `e : t`.
Inference: Given `e`, compute `t` such that `e : t`.

    check (Exp e, Type t): Bool
    infer (Exp e) : Maybe Type

Example implementation:

    check (EInt i, t):
        t == TInt

    check (EDiv e₁ e₂, t):
        check (e₁, t) && check (e₂, t) && (t == TInt || t == TDouble)

    infer (EInt i):
        return TInt

    infer (EDiv e₁ e₂):
        t ← infer (e₁)
        check (e₂, t)
        return t

### Coercion and subtyping

`int` values can be coerced to `double` values.
`int` is a subtype of `double`, written `int ≤ double`
Subtyping `t₁ ≤ t₂` is a preorder, i.e., a reflexive-transitive relation.

    e : t₁
    ------ t₁ ≤ t₂
    e : t₂

Subtyping should be coherent, up-casting via an intermediate type
should not make a difference:

    short int i;
    double x = (double)((int)i);
    double y = (double)i;

In practice, it often does.  E.g. with coercions to `string`:

    int ≤ string             1 → "1"
    int ≤ double ≤ string    1 → 1.0 → "1.0"

Quiz: What is the value of this expression?

    1 + 2 + "hello" + 1 + 2

This should better be a type error!  The value should not depend on
the associativity of "+".

### Elaboration in the type checker

Type checker can insert coercion `ECoerce` when subtyping `int < double` was applied.
```bnfc
internal ECoerce.  Exp ::= "(double)" Exp;
```

Checking/inference: also return elaborated expression.

    check (Exp e, Type t): Maybe Exp
    infer (Exp e) : Maybe (Exp, Type)

Example implementation:

    check (EInt i, t):
        if t == TInt then return (EInt i)
        else fail "expected int"

    check (EDiv e₁ e₂, t):
        assert (t ∈ {TInt, TDouble})
        e₁' ← check (e₁, t)
        e₂' ← check (e₂, t)
        return (EDiv e₁' e₂')

    infer (EInt i):
        return (EInt i, TInt)

    infer (EDiv e₁ e₂):
        (e₁', t₁) ← infer (e₁)
        (e₂', t₂) ← infer (e₂)
        t ← max t₁ t₂
        e₁'' = coerce (e₁', t₁, t)
        e2'' = coerce (e₂', t₂, t)
        return (EDiv e₁'' e₂'', t)

    coerce (e, t₁, t₂):
        if t₁ == TInt and t₂ == TDouble then ECoerce(e)
        else e


Boolean expressions
-------------------

Comparison operators return a boolean:

    e₁ : t    e₂ : t
    ---------------- t ∈ {int, double}
    e₁ < e₂ : bool

    e₁ : t    e₂ : t
    ---------------- t ∈ {bool, int, double}
    e₁ == e₂ : bool


Statements
----------

Statements all have type `void` so we can omit it.
Judgements version 1:

    ⊢ s            Statement s is well-typed
    ⊢ s₁...sₙ      Statement sequence s₁...sₙ is well-typed

NB: ⊢ = "turnstile" (With TeX input mode: \vdash)

Rules for conditional statements:

    e : bool    ⊢ s
    ---------------
    ⊢ while (e) s

    e : bool    ⊢ s₁    ⊢ s₂
    ------------------------
    ⊢ if (e) s₁ else s₂

Statement sequences:

           ⊢ s₀    ⊢ s₁...sₙ
    ---    -----------------
    ⊢ ε    ⊢ s₀ s₁...sₙ

Expressions as statements

    e : t
    -----
    ⊢ e;


Return statements
-----------------

A return statement needs to have a return expression of the correct type.
```c
int main () {
  ...
  if (...) return 5;
  else return 1.0;    // error
}
```
Judgements version 1.5:

    ⊢ᵗ s            Statement s is well-typed, may return t
    ⊢ᵗ s₁...sₙ      Statement sequence s₁...sₙ is well-typed, may return t

Rule:

    e : t
    -----------
    ⊢ᵗ return e;


Checking functions
------------------

```lbnf
DFun.  Def ::= Type Id "(" ")" "{" [Stm] "}"
```

Checking function definitions (version 1):
```
    checkStm (Type t, Stm s)

    checkStms (Type t, [Stm] ss):
       for (Stm s ∈ ss):
           checkStm(t, s)

    checkDef (DFun t x ss):
      checkStms (t, ss)
```


Variables and blocks
--------------------

Typing _environments_ (aka _contexts_) assign types to variables.

Example:
```c
  int f (double x) {
    int i = 2 ;
    int j = 5 ;
    // int i;         // illegal, i already declared in block
    // int x;         // illegal, x already declared in block
    {
      int    x = 0;   // legal, shadows function parameter.
      double i = 3 ;  // shadows the previous variable i
      {
                   // Environment: (x:double,i:int,j:int).(x:int,i:double).()
      }
      i = i + j;   // the inner i becomes 8.0
      int k;
    }
    i++ ;          // the outer i becomes 3
    return i + j;
  }
```

Environments `Γ` are structured as stack of blocks.
Each block `Δ` is a (partial and finite) map from identifier to type.

Example: in the innermost block, the typing environment is a stack of three blocks

  1. `(x:double,i:int,j:int)`: function parameters and top local variables
  2. `(x:int,i:double)`: variables declared in the first inner block
  3. `()`: variables declared in the second inner block (none)

Declaration statements like `int i, j;` declare a new block component `Δ = (i:int, j:int)`.
Notation:

  - `Γ.ε` or `Γ.` or `Γ.()`: Context Γ extended by a new empty block.
  - `Γ,Δ`: The top block of `Γ` is extended by declarations `Δ`, assuming no clash.

E.g. `(x:t),(x:...)` is a clash.

Judgements version 2:

  - `Γ ⊢ e : t`:  In context `Γ`, expression `e` has type `t`.
  - `Γ ⊢ᵗ s ⇒ Δ`   :  In context `Γ`, statement `s` may return `t` and declares `Δ`.
  - `Γ ⊢ᵗ ss ⇒ Δ`  :  (ditto)

Rules for declarations and blocks.

    ------------------------------------- no xᵢ ∈ Δ
    Γ.Δ ⊢ᵗ⁰ t x₁,...,xₙ; ⇒ (x₁:t,...xₙ:t)

    Γ.(Δ,x:t) ⊢ e : t
    ------------------------ x ∉ Δ
    Γ.Δ ⊢ᵗ⁰ t x = e; ⇒ (x:t)

    Γ.() ⊢ᵗ⁰ ss ⇒ Δ
    -----------------
    Γ ⊢ᵗ⁰ { ss } ⇒ ()

Valid but pointless example for initialization statement:
```c
int main () {
  int i = i;
}
```
`t x = e` is sugar for `t x; x = e;`.

Rules for sequences:

    -----------
    Γ ⊢ᵗ⁰ ε ⇒ ()

    Γ ⊢ᵗ⁰ s ⇒ Δ₁    Γ,Δ₁ ⊢ᵗ⁰ ss ⇒ Δ₂
    --------------------------------
    Γ ⊢ᵗ⁰ s ss ⇒ Δ₁,Δ₂

Rules for conditional statements:
Branches need to be in new scope.

    Γ ⊢ᵗ⁰ e : bool    Γ. ⊢ᵗ⁰ s₁ ⇒ Δ₁    Γ. ⊢ᵗ⁰ s₂ ⇒ Δ₂
    -------------------------------------------------
    Γ ⊢ᵗ⁰ if (e) s₁ else e₂ ⇒ ()

    Γ ⊢ᵗ⁰ e : bool    Γ. ⊢ᵗ⁰ s ⇒ Δ
    -----------------------------
    Γ ⊢ᵗ⁰ while (e) s ⇒ ()

Example:
```
    int main () {
      if (condition) int i = 1; else int j = 2;
      return i;
    }
```
Same as `if (condition) { int i = 1; } else { int j = 2; }`.


Functions
---------

When calling a function, we need to provide arguments of the correct type.

Global environment `Σ` (for function _signature_) maps function names to function types.
```c
   bool foo (int x, double y) { ... }
```
Type of `foo` is `bool(int,double)` also written as `(int,double)→bool`.

Judgements version 3:

  - `Σ;Γ ⊢ e : t`:  In signature `Σ` and context `Γ`, expression `e` has type `t`.
  - `Σ;Γ ⊢ᵗ s ⇒ Δ`:  ...
  - `Σ;Γ ⊢ᵗ ss ⇒ Δ`:  ...
  - `Σ ⊢ d`: In signature `Σ`, function definition `d` is well-formed.

Rule for function application:

    Σ;Γ ⊢ e₁ : t₁ ... Σ;Γ ⊢ eₙ : tₙ
    ------------------------------- Σ(f) = t(t₁,...,tₙ)
    Σ;Γ ⊢ f(e₁,...,eₙ) : t

Rule for function definition:

    Σ; (x₁:t₁,...,xₙ:tₙ) ⊢ᵗ ss ⇒ Δ
    ---------------------------------
    Σ ⊢ t f (t₁ x₁, ... tₙ xₙ) { ss }

The correctness of the signature, `Σ(f) = t(t₁,...,tₙ)`, can be assumed in the last rule.

Programs
--------

A program is a list of functions. Checking a program:

- Pass 1: Compute the signature `Σ` (check for duplicate function definitions!).
- Pass 2: In `Σ`, check the body `ss` of each function definition.

The elaborating type checker will compute a new body `ss'` for each
function definition, resulting in an elaborated program.

The result of type checking is a map from function names to their
types and elaborated definitions.


More type annotations
---------------------

Elaboration:
- insert coercions (already discussed)
- disambiguate overloaded operators `+`,`*`,...`<`,`<=`,...,`==`,...
- ...

Type-annotating checker: annotate each subexpression with its type.

- Alternative 1 (book): put an annotation at each subexpression
  `internal ECast.  Exp ::= "(" Type ")" Exp;`

  ```
    infer (Context Γ, Exp e): Maybe Exp

    infer (Γ, EDiv e₁ e₂):
      ECast t₁ e₁' ← infer (Γ, e₁)
      ECast t₂ e₂' ← infer (Γ, e₂)
      t = max t₁ t₂
      return (ECast t (EDiv (ECast t₁ e₁') (ECast t₂ e₂')))
  ```

- Alternative 2: design a new abstract syntax which type-annotations
  in the needed places.
  ```lbnf
  ...
  TEDiv.     TExp ::= "div" Type TExp TExp;

  TSExp.     TStm ::= Type TExp ";" ;
  TSReturn.  TStm ::= "return" Type TExp;
  ...
  ```
