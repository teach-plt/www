Programming Language Technology
DAT151/DIT231
Andreas Abel

Interpretation
==============

Introduction
------------

Why an interpreter if we can have a compiler?
- Defines the meaning of the language.
- Can serve as a reference implementation.
- (In the end, of a compilation chain, there is always _some_ interpreter.)

An interpreter should be _compositional_!

We interpret _type-checked_ programs only, since the meaning of
overloaded operators depends on their type.


Evaluation of arithmetic expressions
------------------------------------

In analogy to type-checking `Γ ⊢ e : t` we have judgement

  `γ ⊢ e ⇓ v`  in environment `γ`, expression `e` has value `v`

Environments `γ` are similar to contexts `Γ`, but instead of mapping
variables `x` to types `t`, they map variables to _values_ `v`.

An environment `γ` is a stack of blocks `δ`, which are finite maps
from variables `x` to values `v`.

Values are integer, floating-point, and boolean literals, or a special
value `null` for being undefined.

This would be a LBNF grammar for our values:
```bnfc
VInt.    Value ::= Integer;
VDouble. Value ::= Double;
VBool.   Value ::= Bool;
VNull.   Value ::= "null";

rules    Bool  ::= "true" | "false";
```
(It is possible but not necessary to use BNFC to generate the value
representation.)

Variable rule.

    ---------- γ(x) = v
    γ ⊢ x ⇓ v

Note that we take the value of the "topmost" `x` --- it may appear in
several blocks `δ` of `γ`.

```c
{ double z = 3.14;       // (z=3.14)
  int    y = 1;          // (z=3.14,y=1)
  { int z = 0;           // (z=3.14,y=1).(z=0)
    int x = y + z;       // (z=3.14,y=1).(z=0,x=1)
    printInt(x);         // 1
  }
}
```

The meaning of the arithmetic operators depends on the static _type_.

    γ ⊢ e₁ ⇓ v₁    γ ⊢ e₂ ⇓ v₂
    --------------------------- v = divide(t,v₁,v₂)
    γ ⊢ e₁ /ₜ e₂ ⇓ v

- `divide(int,v₁,v₂)` is integer division of the integer literals `v₁` by `v₂`.
- `divide(double,v₁,v₂)` is floating-point division of the floating-point
  literals `v₁` by `v₂`.  Integer literals will be converted to floating-point
  first.
- `divide(bool,v₁,v₂)` is undefined.

### Implementation

Judgement `γ ⊢ e ⇓ v` should be read as a function with inputs `γ` and
`e` and output `v`.
```
    eval(Env γ, Exp e): Value

    eval(γ, EId x)        = lookup(γ,x)
    eval(γ, EInt i)       = VInt i
    eval(γ, EDouble d)    = VDouble d
    eval(γ, EDiv t e₁ e₂) = divide(t, eval(γ,e₁), eval(γ,e₂))
```
In the last clause, `eval(γ,e₁)` and `eval(γ,e₂)` can be run in any
order, even in parallel!


Effects
-------

### Assignment

Expression forms like increment (`++x` and `x++`) and decrement and
assignment `x = e` in general _change_ the values of variables.
This is called a _(side) effect_.
(In contrast, the _type_ of variables never changes. Typing has no effects.)

We need to _update_ the environment.
We return the updated environment along with the value:

  `γ ⊢ e ⇓ ⟨v; γ'⟩`  "In environment `γ`, expression `e` has value `v`
                      and updates environment to `γ'`."

We write `γ[x=v]` for environment `γ'` with `γ'(x) = v` and
`γ'(y) = γ(y)` when `y ≠ x`.

Note that we update the "topmost" `x` only --- it may appear in
several blocks `δ` of `γ`.

Rules.

    -------------- γ(x) = v
    γ ⊢ x ⇓ ⟨v; γ⟩

    -------------------------------- γ(x) = i
    γ ⊢ ++(int)x ⇓ ⟨i+1; γ[x = i+1]⟩

    γ ⊢ e ⇓ ⟨v; γ'⟩
    --------------------------
    γ ⊢ (x = e) ⇓ ⟨v; γ'[x=v]⟩

    γ ⊢ e₁ ⇓ ⟨v₁;γ₁⟩    γ₁ ⊢ e₂ ⇓ ⟨v₂;γ₂⟩
    ------------------------------------ v = divide(t,v₁,v₂)
    γ ⊢ e₁ /ₜ e₂ ⇓ ⟨v;γ₂⟩

The last rule show: we need to _thread_ the environment through the
judgements.

The evaluation _order_ matters now!

```
    eval(Env γ, Exp e): Value × Env

    eval(γ, EId x)        = ⟨ lookup(γ,x), γ ⟩

    eval(γ, EAssign x e)  = let
        ⟨v,γ'⟩ = eval(γ,e)
      in ⟨ v, update(γ',x,v) ⟩

    eval(γ, EDiv t e₁ e₂) = let
        ⟨v₁,γ₁⟩ = eval(γ, e₁)
        ⟨v₂,γ₂⟩ = eval(γ₁,e₂)
      in ⟨ divide(t,v₁,v₂), γ₂ ⟩
```
Consider `x=0 ⊢ x + x++` vs `x=0 ⊢ x++ + x`.


### State threading behind the scenes

E.g. in Java, we can use a environment `env` global to the interpreter and
mutate it with update.
```
    eval(Exp e): Value

    eval(EId x): return env.lookup(x)

    eval(EAssign x e):
        v ← eval(e)
        env.update(x,v)
        return v

    eval(γ, EDiv t e₁ e₂):
        v₁ ← eval(e₁)
        v₂ ← eval(e₂)
        return divide(t,v₁,v₂)
```
This keeps the environment _implicit_.

In Haskell, we can use the _state monad_ for the same purpose.
```haskell
import Control.Monad.State (State, get, modify, evalState)

eval : Exp → State Env Value

eval (EId x) = do
  γ ← get
  lookupVar γ x

eval (EAssign x e) = do
  v ← eval e
  modify (λ γ → updateVar γ x v)
  return v

eval (EDiv TInt e₁ e₂) = do
  VInt i₁ ← eval e₁
  VInt i₂ ← eval e₂
  return (VInt (div i₁ i₂))

interpret : Exp → Value
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

### I/O

Input and output:

    ---------------------- read i from stdin
    γ ⊢ readInt() ⇓ ⟨i, γ⟩

    γ ⊢ e ⇓ ⟨i, γ'⟩
    ---------------------------- print i on stdout
    γ ⊢ printInt(e) ⇓ ⟨null, γ'⟩

We do not model input and output mathematically here.
Note that `read i from stdin` can also abort with an exception.

Exercise: Could we model evaluation with input and output as a
mathematical function?
Hint:
- The input and the output both could be a stream of strings.
- Exceptions need to propagate.  (See `return` statement below.)


Shortcutting
------------

What is wrong with this rule?

    γ ⊢ e₁ ⇓ ⟨b₁;γ₁⟩    γ₁ ⊢ e₂ ⇓ ⟨b₂;γ₂⟩
    ------------------------------------ b = b₁ ∧ b₂
    γ ⊢ e₁ && e₂ ⇓ ⟨b;γ₂⟩

Here are some examples where we would like to _shortcut_ computation:
```c
  int b = 1;
  if (b == 0 && the_goldbach_conjecture_holds_up_to_10E100) { ... }
  int x = 0 * number_of_atoms_on_the_moon;
```

Short-cutting logical operators like `&&` is essentially used in C, e.g.:
```
  if (p != NULL && p[0] > 10)
```
Without short-cutting, the program would crash in `p[0]` by accessing
a `NULL` pointer.

Rules with short-cutting:

    γ ⊢ e₁ ⇓ ⟨false, γ₁⟩
    -------------------------
    γ ⊢ e₁ && e₂ ⇓ ⟨false, γ₂⟩

    γ ⊢ e₁ ⇓ ⟨true, γ₁⟩    γ₁ ⊢ e₂ ⇓ ⟨b, γ₂⟩
    ----------------------------------------
    γ ⊢ e₁ && e₂ ⇓ ⟨b;γ₂⟩

The effects of `e₂` are not executed if `e₁` already evaluated to `false`.

Example:
```c
  while (x < 10 && f(x++)) { ... }
```

You can circumvent this by defining your own `and`:
```c
  bool and(bool x, bool y) { return x && y; }

  while (and(x < 10, f(x++))) { ... }
```

Digression on:  call-by-name  (call-by-need)  call-by-value
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
- call-by-value: `... = double (3) = 0`
- call-by-name:  `... = 0`
- call-by-need:  `... = let x=1+2 in 0 = 0`


Languages with effects (such as C/C++) mostly use call-by-value.
Haskell is pure (no effects) and uses call-by-need, which refines call-by-name.



Statements
----------

Statements do not have a value.  They just have effects.

    γ ⊢ s  ⇓ γ'
    γ ⊢ ss ⇓ γ'

Sequencing `γ ⊢ ss ⇓ γ'`:

    γ ⊢ ε ⇓ γ

    γ ⊢ s ⇓ γ₁    γ₁ ⊢ ss ⇓ γ₂
    ---------------------------
    γ ⊢ s ss ⇓ γ₂

An empty sequence is interpreted as the identity function `id : Env → Env`,
sequence composition as function composition.

Blocks.

    γ. ⊢ ss ⇓ γ'.δ
    --------------
    γ ⊢ {ss} ⇓ γ'

Variable declaration.

    γ.δ[x=null] ⊢ e ⇓ ⟨v, γ'.δ'⟩
    ----------------------------
    γ.δ ⊢ t x = e;  ⇓ γ'.δ'[x=v]

While.

    γ ⊢ e ⇓ ⟨false; γ'⟩
    --------------------
    γ ⊢ while (e) s ⇓ γ'

    γ ⊢ e ⇓ ⟨true; γ₁⟩    γ₁. ⊢ s ⇓ γ₂.δ    γ₂ ⊢ while e (s) ⇓ γ₃
    -------------------------------------------------------------
    γ ⊢ while (e) s ⇓ γ₃

Or:

    γ ⊢ e ⇓ ⟨true; γ₁⟩    γ₁ ⊢ { s } while e (s) ⇓ γ₃
    -------------------------------------------------------------
    γ ⊢ while (e) s ⇓ γ₃

Return?

    γ ⊢ e ⇓ ⟨v, γ'⟩
    -------------------
    γ ⊢ return e; ⇓ ??

Return alters the control flow:
We can exit from the middle of a loop, block etc.!
```c
bool prime (int p) {
  if (p <= 2) return p == 2;
  else {
    int q = 3;
    while (q * q <= p) {
      if (divides(q,p)) return false;
      else q = q + 2;
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

    γ ⊢ s ⇓ r
    r ::= v | γ'

The disjoint union `Value ⊎ Env` could be modeled in LBNF as:
```lbnf
Return.   Result ::= "return" Value;
Continue. Result ::= "continue" Env;
```

Return.

    γ ⊢ e ⇓ ⟨v, γ'⟩
    ------------------
    γ ⊢ return e; ⇓ v

Statement as expression.

    γ ⊢ e ⇓ ⟨v, γ'⟩
    ---------------
    γ ⊢ e; ⇓ γ'

Sequence: Propagate exception.

    γ ⊢ s ⇓ v
    -------------
    γ ⊢ s ss ⇓ v

    γ ⊢ s ⇓ γ₁   γ₁ ⊢ ss ⇓ r
    -------------------------
    γ ⊢ s ss ⇓ r

Exercise:  How to change `while`?
One solution:

    γ ⊢ e ⇓ ⟨true; γ₁⟩    γ₁ ⊢ { s } while e (s) ⇓ r
    -------------------------------------------------------------
    γ ⊢ while (e) s ⇓ r

Function call.

    γ    ⊢ e₁ ⇓ ⟨v₁, γ₁⟩
    γ₁   ⊢ e₂ ⇓ ⟨v₂, γ₂⟩
    ...
    γₙ₋₁ ⊢ eₙ ⇓ ⟨vₙ, γₙ⟩
    (x₁=v₁,...,xₙ=vₙ) ⊢ ss ⇓ v
    ------------------------------- σ(f) = t f (t₁ x₁,...,tₙ xₙ) { ss }
    γ ⊢ f(e₁,...,eₙ) ⇓ ⟨v, γₙ⟩

To implement the side condition, we need a global map `σ` from
function names `f` to their definition `t f (t₁ x₁,...,tₙ xₙ) { ss }`.

### Implementation

In Java, we can use Java's exception mechanism.
```java

   eval(ECall f es):
     vs ← eval(es)
     (Δ,ss) ← lookupFun(f)
     γ ← saveEnv()
     setEnv(makeEnv(Δ,vs))
     try {
        execStms(ss)
     } catch {
       ReturnException v:
         setEnv(γ)
         return v
     }

   execStm(SReturn e):
     v ← eval(e)
     throw new ReturnException(v)
```
In Haskell, we can use the _exception monad_.
```haskell
import Control.Monad.Except
import Control.Monad.Reader
import Control.Monad.State

type M = ReaderT Sig (StateT Env (ExceptT Value IO))

eval :: Exp → M Value
eval (ECall f es) = do
  vs ← mapM eval es
  (Δ,ss) ← lookupFun f
  γ ← get
  put (makeEnv Δ vs)
  execStms ss `catchError` λ v → do
    put γ
    return v

execStm :: Stm → M ()
execStm (SReturn e) = do
  v ← eval e
  throwError v
```
More in the hands-on lecture...


Programs
--------

A program is interpreted by executing the statements of the `main`
function in an environment with just one empty block.
