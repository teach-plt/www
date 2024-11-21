---
title: Interpretation
subtitle: Programming Language Technology, DAT151/DIT231
---

Introduction
------------

Why an interpreter if we can have a compiler?
- (In the end of a compilation chain, there is always _some_ interpreter.)
- Defines the meaning of the language.
- Can serve as a reference implementation.
- Can help _boot strapping_ (self-implementating) a language.
  1. Write interpreter `I` for new language `X` in existing language `Y`.
  2. Write simple compiler `C` for `X` in `X`.
  3. Run (via `I`) `C` on `C` to get a compiled compiler `C'`.
  4. Write an optimizing compiler `O` for `X` in `X`.
  5. Run `C'` on `O` to get an a compiled optimizing compiler `Cₒ`.
  6. Run `Cₒ` on `Cₒ` to get an optimized optimizing compiler `Cₒ'`.

An interpreter should be _compositional_!

> ⟦op (e₁, e₂)⟧ = ⟦op⟧ (⟦e₁⟧, ⟦e₂⟧)
>
> ```
>   eval (Op(e₁,e₂)):
>     v₁ ← eval e₁
>     v₂ ← eval e₂
>     v  ← ...combine v₁ and v₂ according to Op...
>     return v
> ```

An interpreter can be specified by:
1. A mathematical function `⟦_⟧ : Exp → Val` (_set theory_, _domain theory_).
2. A relation `_⇓_ ⊆ Exp × Val` (_big-step operational semantics_).
3. A reduction relation `_→_ ⊆ Exp × Exp` (_small-step operational semantics_).
4. Pseudo code.
5. A reference implementation.

We interpret _type-checked_ programs only, since the meaning of
overloaded operators depends on their type.

(To avoid clutter, we reuse the names of the untyped ASTs, e.g. `Exp` instead `ExpT` etc.)

Evaluation of arithmetic expressions
------------------------------------

In analogy to type-checking `Γ ⊢ e : t` we have judgement

>   `γ ⊢ e ⇓ v`
>
>   "In environment `γ`, expression `e` has value `v`."

Environments `γ` are similar to contexts `Γ`, but instead of mapping
variables `x` to types `t`, they map variables to _values_ `v`.

An environment `γ` is a stack of blocks `δ`, which are finite maps
from variables `x` to values `v`.

Values are integer, floating-point, and boolean literals, or a special
value `null` for being undefined.

This would be an LBNF grammar for our values:
```lbnf
    VInt.    Val  ::= Integer;
    VDouble. Val  ::= Double;
    VBool.   Val  ::= Bool;
    VNull.   Val  ::= "null";

    BTrue.   Bool ::= "true";
    BFalse.  Bool ::= "false";
```
(It is possible but not necessary to use BNFC to generate the value
representation.)

Variable rule.

> ```
>   ---------- γ(x) = v
>   γ ⊢ x ⇓ v
> ```

Note that we take the value of the "topmost" `x`---it may appear in
several blocks `δ` of `γ`.

```c
    { double z = 3.14;       // (z=3.14)
      int    y = 1;          // (z=3.14,y=1)
      { int x = 0;           // (z=3.14,y=1).(x=0)
        int z = x + y;       // (z=3.14,y=1).(x=0,z=1)
        printInt(z);         // 1
      }
      printDouble(z);        // 3.14
    }
```

The meaning of an arithmetic operator depends on its static _type_.

> ```
>   γ ⊢ e₁ ⇓ v₁    γ ⊢ e₂ ⇓ v₂
>   --------------------------- v = divide(t,v₁,v₂)
>   γ ⊢ e₁ /ₜ e₂ ⇓ v
> ```

- `divide(int,v₁,v₂)` is integer division of the integer literals `v₁` by `v₂`.
- `divide(double,v₁,v₂)` is floating-point division of the floating-point
  literals `v₁` by `v₂`.  Integer literals will be converted to floating-point
  first.
- `divide(bool,v₁,v₂)` is undefined.


### Implementation

Judgement `γ ⊢ e ⇓ v` should be read as a function with inputs `γ` and
`e` and output `v`.
```
    eval(Env γ, Exp e): Val

    eval(γ, EId x)        = lookup(γ,x)
    eval(γ, EInt i)       = VInt i
    eval(γ, EDouble d)    = VDouble d
    eval(γ, EDiv t e₁ e₂) = divide(t, eval(γ,e₁), eval(γ,e₂))
```
In the last clause, `eval(γ,e₁)` and `eval(γ,e₂)` can be run in any
order, even in parallel!

### Correctness properties

- Type soundness (weak correctness):
  > If `e : t` and `e ⇓ v` then `v : t`.

  "If expression `e` has type `t` and `e` evaluates to value `v` then `v` also has type `t`."

- Termination (strong correctness):
  > If `e : t` then `e ⇓ v` for some `v : t`.

- Allowing non-termination:
  > If `e : t` then either evaluation of `e` diverges, or `e ⇓ v` with `v : t`.

Quiz:

- Which of these hold in exceptional cases?
  * Overflow
  * Division by zero

- Is this definition type-sound?
  ```
    eval(γ, EDiv double e₁ e₂) = VDouble 0.0
  ```


Effects
-------

### Assignment

Expression forms like increment (`++x` and `x++`) and decrement and
assignment `x = e` in general _change_ the values of variables.
This is called a _(side) effect_.
(In contrast, the _type_ of variables never changes. Typing has no effects.)

We need to _update_ the environment.
We return the updated environment along with the value:

>   `γ ⊢ e ⇓ ⟨v; γ'⟩`
>
>   "In environment `γ`, expression `e` has value `v`
>    and updates environment to `γ'`."

We write `γ[x=v]` for updating the _topmost_ (innermost) occurrence of `x` in environment `γ'` to value `v`.

(Note `x` may appear in several blocks `δ` of `γ`.)

Rules.

> ```
>   -------------- γ(x) = v
>   γ ⊢ x ⇓ ⟨v; γ⟩
>
>   -------------------------------- γ(x) = i
>   γ ⊢ ++(int)x ⇓ ⟨i+1; γ[x = i+1]⟩
>
>   γ ⊢ e ⇓ ⟨v; γ'⟩
>   --------------------------
>   γ ⊢ (x = e) ⇓ ⟨v; γ'[x=v]⟩
>
>   γ ⊢ e₁ ⇓ ⟨v₁;γ₁⟩    γ₁ ⊢ e₂ ⇓ ⟨v₂;γ₂⟩
>   ------------------------------------- v = divide(t,v₁,v₂)
>   γ ⊢ e₁ /ₜ e₂ ⇓ ⟨v;γ₂⟩
> ```

The last rule shows: we need to _thread_ the environment through the
judgements.
```
    eval(Env γ, Exp e): Val × Env

    eval(γ, EId x)        =
      ⟨ lookup(γ,x), γ ⟩

    eval(γ, EAssign x e)  = let
         ⟨v,γ'⟩ = eval(γ,e)
      in ⟨ v, update(γ',x,v) ⟩

    eval(γ, EDiv t e₁ e₂) = let
         ⟨v₁,γ₁⟩ = eval(γ, e₁)
         ⟨v₂,γ₂⟩ = eval(γ₁,e₂)
      in ⟨ divide(t,v₁,v₂), γ₂ ⟩
```


The evaluation _order_ matters now!

Consider
```
    x=0 ⊢ x + x++
```
vs.
```
    x=0 ⊢ x++ + x.
```

### State threading behind the scenes

E.g. in Java, we can use an environment `env` global to the interpreter and
mutate it with update.
```
    eval(Exp e): Val

    eval(EId x):
        return env.lookup(x)

    eval(EAssign x e):
        v ← eval(e)
        env.update(x,v)
        return v

    eval(EDiv t e₁ e₂):
        v₁ ← eval(e₁)
        v₂ ← eval(e₂)
        return divide(t,v₁,v₂)
```
This keeps the environment _implicit_.
(But careful with function calls, see below!)

In Haskell, we can use the _state monad_ for the same purpose.
```haskell
    import Control.Monad.State (State, get, modify, evalState)

    eval : Exp → State Env Val

    eval (EId x) = do
      γ ← get
      return (lookupVar γ x)

    eval (EAssign x e) = do
      v ← eval e
      modify (λ γ → updateVar γ x v)
      return v

    eval (EDiv TInt e₁ e₂) = do
      VInt i₁ ← eval e₁
      VInt i₂ ← eval e₂
      return (VInt (div i₁ i₂))

    interpret : Exp → Val
    interpret e = evalState (eval e) emptyEnv
```

The Haskell state monad is just sugar.  It is implemented roughly by:
```haskell
    type State s a = s → (a, s)

    get :: State s s                 -- s → (s, s)
    get s = (s, s)

    modify :: (s → s) → State s ()   -- s → ((), s)
    modify f s = ((), f s)

    return :: a → State s a          -- s → (a, s)
    return a s = (a, s)

    (do x ← p; q(x)) s = let
         (a, s₁) = p s
         (b, s₂) = q(a) s₁
      in (b, s₂)
```


### Correctness revisited

We need to start with _good_ environments `γ : Γ`.
This is defined pointwise: `γ(x) : Γ(x)` for all `x` in scope.

- Type soundness (weak correctness):
  > If `Γ ⊢ e : t` and `γ : Γ` and `γ ⊢ e ⇓ ⟨v; γ'⟩` then `v : t` and `γ' : Γ`.

  "If expression `e` has type `t` in context `Γ` and `γ` is an environment matching `Γ` and `e` evaluates to value `v` and `γ'` then `v` also has type `t` and `γ'` also matches `Γ`."

- Termination (strong correctness):
  > If `Γ ⊢ e : t` and `γ : Γ` then `γ ⊢ e ⇓ ⟨v; γ'⟩` for some `v : t` and `γ' : Γ`.

- Allowing non-termination:
  ... (Exercise)


### I/O

Input and output:

> ```
>   ---------------------- read i from stdin
>   γ ⊢ readInt() ⇓ ⟨i; γ⟩
>
>   γ ⊢ e ⇓ ⟨i; γ'⟩
>   ---------------------------- print i on stdout
>   γ ⊢ printInt(e) ⇓ ⟨null; γ'⟩
> ```

We do not model input and output mathematically here.
Note that `read i from stdin` can also abort with an exception.

Bonus exercise: Could we model evaluation with input and output as a
mathematical function?
Hint:
- The input and the output both could be a stream of strings.
- Exceptions need to propagate.  (See `return` statement below.)


Statements
----------

Statements do not have a value.  They just have effects.

>   `γ ⊢ s  ⇓ γ'`
>
>   `γ ⊢ ss ⇓ γ'`
>
>   "In environment `γ`, statement `s` (sequence `ss`) executes
>    successfully, returning updated environment `γ'`."

Sequencing `γ ⊢ ss ⇓ γ'`:

> ```
>   γ ⊢ ε ⇓ γ
>
>   γ ⊢ s ⇓ γ₁    γ₁ ⊢ ss ⇓ γ₂
>   ---------------------------
>   γ ⊢ s ss ⇓ γ₂
> ```

An empty sequence is interpreted as the identity function `id : Env → Env`,
sequence composition as function composition.

Blocks.

> ```
>   γ. ⊢ ss ⇓ γ'.δ
>   --------------
>   γ ⊢ {ss} ⇓ γ'
> ```

Variable declaration.

> ```
>   γ.δ[x=null] ⊢ e ⇓ ⟨v, γ'.δ'⟩
>   ----------------------------
>   γ.δ ⊢ t x = e;  ⇓ γ'.δ'[x=v]
> ```

While.

> ```
>   γ ⊢ e ⇓ ⟨false; γ'⟩
>   --------------------
>   γ ⊢ while (e) s ⇓ γ'
>
>   γ ⊢ e ⇓ ⟨true; γ₁⟩    γ₁. ⊢ s ⇓ γ₂.δ    γ₂ ⊢ while e (s) ⇓ γ₃
>   -------------------------------------------------------------
>   γ ⊢ while (e) s ⇓ γ₃
> ```

Or:

> ```
>   γ ⊢ e ⇓ ⟨true; γ₁⟩    γ₁ ⊢ { s } while e (s) ⇓ γ₃
>   -------------------------------------------------
>   γ ⊢ while (e) s ⇓ γ₃
> ```

Return?

> ```
>   γ ⊢ e ⇓ ⟨v, γ'⟩
>   -------------------
>   γ ⊢ return e; ⇓ ??
> ```

Return alters the control flow:
We can exit from the middle of a loop, block etc.!
```c
    bool prime (int p) {
      if (p <= 2) return p == 2;
      if (divides(2,p)) return false;
      {
        int q = 3;
        while (q * q <= p) {
          if (divides(q,p)) return false;
          q = q + 2;
        }
      }
      return true;
    }
```
Similar to `return`: `break`, `continue`.


Exceptions
----------

`return` can be modelled as an _exception_ that carries the return
value `v` as exception information.
When calling a function, we handle the exception, treating it as
regular return from the function.

>    `γ ⊢ s ⇓ r`  where  `r ::= v | γ'`
>
>    "In environment `γ` statement `s` executes successfully,
>     either asking to return value `v`,
>     or to continue execution in updated environment `γ'`."

The disjoint union `Val ⊎ Env` could be modeled in LBNF as:
```lbnf
    Return.   Result ::= "return" Val;
    Continue. Result ::= "continue" Env;
```

Return.

> ```
>   γ ⊢ e ⇓ ⟨v, γ'⟩
>   ------------------
>   γ ⊢ return e; ⇓ v
> ```

Statement as expression.

> ```
>   γ ⊢ e ⇓ ⟨v, γ'⟩
>   ---------------
>   γ ⊢ e; ⇓ γ'
> ```

Sequence: Propagate exception.

> ```
>   γ ⊢ s ⇓ v
>   -------------
>   γ ⊢ s ss ⇓ v
>
>   γ ⊢ s ⇓ γ₁   γ₁ ⊢ ss ⇓ r
>   -------------------------
>   γ ⊢ s ss ⇓ r
> ```

Exercise:  How to change `while`?
One solution:

> ```
>   γ ⊢ e ⇓ ⟨true; γ₁⟩    γ₁ ⊢ { s } while e (s) ⇓ r
>   ------------------------------------------------
>   γ ⊢ while (e) s ⇓ r
> ```

Function call.

> ```
>   γ    ⊢ e₁ ⇓ ⟨v₁, γ₁⟩
>   γ₁   ⊢ e₂ ⇓ ⟨v₂, γ₂⟩
>   ...
>   γₙ₋₁ ⊢ eₙ ⇓ ⟨vₙ, γₙ⟩
>   (x₁=v₁,...,xₙ=vₙ) ⊢ ss ⇓ v
>   -------------------------- σ(f) = t f (t₁ x₁,...,tₙ xₙ) { ss }
>   γ ⊢ f(e₁,...,eₙ) ⇓ ⟨v, γₙ⟩
> ```

To implement the side condition, we need a global map `σ` from
function names `f` to their definition `t f (t₁ x₁,...,tₙ xₙ) { ss }`.

Quiz: In the last rule, what if `ss` does not contain a `return` statement?

### Implementation

In Java, we can use Java's exception mechanism.
```java

   eval(ECall f es):
     vs     ← eval(es)        // Evaluate list of arguments
     (Δ,ss) ← lookupFun(f)    // Get parameters and body of function
     γ      ← saveEnv()       // Save current environment
     setEnv(makeEnv(Δ,vs))    // New environment binding the parameters
     try {
        execStms(ss)          // Run function body
     } catch {
       ReturnException v:
         setEnv(γ)            // Restore environment
         return v             // Evaluation result is returned value
     }

   execStm(SReturn e):
     v ← eval(e)
     throw new ReturnException(v)
```
In Haskell, we can use the _exception monad_.
```haskell
    import Control.Monad
    import Control.Monad.Except
    import Control.Monad.Reader
    import Control.Monad.State

    type M = ReaderT Sig (StateT Env (ExceptT Val IO))

    eval :: Exp → M Val
    eval (ECall f es) = do
      vs     ← mapM eval es
      (Δ,ss) ← lookupFun f
      γ      ← get
      put (makeEnv Δ vs)
      execStms ss `catchError` λ v → do
        put γ
        return v

    execStm :: Stm → M ()
    execStm (SReturn e) = do
      v ← eval e
      throwError v
```

N.B.: `makeEnv (x₁:t₁, ..., xₙ:tₙ) (v₁, ..., vₙ) = (x₁=v₁, ... xₙ=tₙ)`.


Programs
--------

A program is interpreted by executing the statements of the `main`
function in an environment with just one empty block.



Shortcutting
------------

What is wrong with this rule?

> ```
>   γ ⊢ e₁ ⇓ ⟨b₁;γ₁⟩    γ₁ ⊢ e₂ ⇓ ⟨b₂;γ₂⟩
>   ------------------------------------ b = b₁ ∧ b₂
>   γ ⊢ e₁ && e₂ ⇓ ⟨b;γ₂⟩
> ```

Here are some examples where we would like to _shortcut_ computation:
```c
    int b = 1;
    if (b == 0 && the_goldbach_conjecture_holds_up_to_10E100) { ... }
    int x = 0 * number_of_atoms_on_the_moon;
```

Short-cutting logical operators like `&&` is essentially used in C, e.g.:
```c
    if (p != NULL && p[0] > 10)
```
Without short-cutting, the program would crash in `p[0]` by accessing
a `NULL` pointer.

Rules with short-cutting:

> ```
>   γ ⊢ e₁ ⇓ ⟨false; γ₁⟩
>   -------------------------
>   γ ⊢ e₁ && e₂ ⇓ ⟨false; γ₁⟩
>
>   γ ⊢ e₁ ⇓ ⟨true; γ₁⟩    γ₁ ⊢ e₂ ⇓ ⟨b; γ₂⟩
>   ----------------------------------------
>   γ ⊢ e₁ && e₂ ⇓ ⟨b; γ₂⟩
> ```

The effects of `e₂` are not executed if `e₁` already evaluated to `false`.

Example:
```c
  while (x < 10 && f(x++)) { ... }
```

You can circumvent this by defining your own `and`:
```c
  bool and (bool x, bool y) { return x && y; }

  while (and (x < 10, f(x++))) { ... }
```

### Digression:  call-by-name / call-by-need /  call-by-value

(This is properly covered in the lecture on functional languages and in lab 4.)

- call-by-value: evaluate function arguments, pass values to function
- call-by-name: pass expressions to function unevaluated (substitution)
- call-by-need: like call-by-name, only evaluate each argument when it
  is used the first time.
  If it is used a second time, reuse the value computed the first time.

Example 1:
```
    double (x) = x + x
    double (1+2)
```
- call-by-value: `... = double (3) = 3 + 3 = 6`
- call-by-name:  `... = (1+2) + (1+2) = 3 + (1+2) = 3 + 3 = 6`
- call-by-need:  `... = let x=1+2 in x + x = let x = 3 in x + x = 3 + 3 = 6`

Example 2:
```
    zero (x) = 0
    zero (1+2)
```
- call-by-value: `... = zero (3) = 0`
- call-by-name:  `... = 0`
- call-by-need:  `... = let x=1+2 in 0 = 0`


Languages with effects (such as C/C++) mostly use call-by-value.
Haskell is pure (no effects) and uses call-by-need, which refines call-by-name.

---
Andreas Abel, updated 2023-11-16
