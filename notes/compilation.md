---
title: Compiling C-- to JVM
subtitle: Programming Language Technology, DAT151/DIT231
---


(Additional notes complementing the slides.)

Tasks of the compiler
---------------------

0. Produce binary code or symbolic assembly.
   - We produce symbolic JVM that gets assembled into a `.class` file by `jasmin`.

1. Translate variable names to addresses
   - Subtask: Compute space needed to hold local variables

2. Translate expression trees to stack instruction sequences
   - Subtask: Compute stack space needed to run stack instructions

3. Translate control structures and boolean operations to jumps

4. Translate function calls into machine-level calls

Infrastructure of the compiler
------------------------------

Signature (global symbol table):
- name and type of functions suitable for Jasmin
- Jasmin types:
  * `I`: `int`
  * `D`: `double`
  * `V`: `void`
  * `Z`: `boolean`

Context (local symbol table):
1. allocate local variables (`newLocal`)
2. resolve variable names to addresses (`lookupVar`)
3. free variables when exiting a block (`newBlock`, `popBlock`)

State
1. generate new labels
2. manage local variable context
3. keep track of maximum stack use
4. append generated code (`emit`)

The `emit` function:
- responsible for
  * outputting the generated code
  * keeping track of stack usage
- pass _abstract_ JVM syntax to `emit`!
- printing abstract JVM can select optimal instruction for e.g. "load constant"
  * `iconst` if ≥ -1 and ≤ 5      (takes 1 byte)
  * `bipush` if ≥ -128 and ≤ 127  (takes 2 bytes)
  * `ldc`    otherwise            (takes ~ 5 bytes)
- you can "invent" abstract JVM instructions that can be printed easily
  to a sequence of concrete JVM instructions

We specify the compiler by so-called __compilation schemes__ (pseudo-code describing syntax-directed traversal).

Compiling expressions
---------------------

An expression of type `t` is compiled to instructions whose effect is
to leave a new value of type `t` on top of the stack, and leave the
rest of the stack unchanged.

```haskell
  compileExp(EInt i):
    emit(iconst i)

  compileExp(EAdd t e₁ e₂):
    compileExp(e₁)
    compileExp(e₂)
    emit(t-add)            -- either iadd or dadd

  compileExp(EVar x):
    a <- lookupVar
    emit(t-load a)         -- either iload or dload

  compileExp(ECall x es)
    for (e ∈ es):          -- compile function arguments
      compileExp(e)
    f <- lookupFun(x)      -- get the Jasmin name of function x
    emit(invokestatic f)   -- emit function call

  compileExp(EAss x e):
    a <- lookupVar x
    compileExp(e)
    emit(t-store a)        -- either istore or dstore
    -- Problem here
```
(We omit `emit` in the following.)

## Compiler correctness

JVM Small-step semantics without jumps:
```
    i : ⟨V,S⟩ → ⟨V',S'⟩

    i      : JVM instruction (or instruction sequence)
    V / V' : variable store before/after
    S / S' : stack before/after
```
We say `γ ~ V`  if environment `γ` translates to variable store `V`.

Correctness statement (simplified):

>  If         γ ⊢ e ⇓ ⟨v, γ'⟩
>  and        γ ~ V
>  then       compileExp(e) : ⟨V,S⟩ →* ⟨V',S.v⟩
>  such that  γ' ~ V'.

Correct translation of assignment:
```haskell
  compileExp(EAss x e):
    a <- lookupVar x
    compileExp(e)
    emit(t-store a)
    emit(t-load a)
```

Compiling statements
--------------------

A statement (sequence) is compiled to instructions whose effect on the
stack is nil (no change).

```haskell
  compileStm(SInit t x e):
    a <- newLocal t x
    compileExp(e)
    t-store a              -- either istore or dstore

  compileStm(SExp t e):
    compileExp(e)
    t-pop                  -- either pop or pop2

  compileStm(SBlock ss):
    newBlock
    for (s : ss)
      compileStm(s)
    popBlock
```

Correctness statement (simplified):

  If         γ ⊢ s ⇓ γ'
  and        γ ~ V
  then       compileStm(s) : ⟨V,S⟩ →* ⟨V',S⟩
  such that  γ' ~ V'.

Compiling booleans
------------------

The simplest compiler will compile boolean expressions to instructions
that leave either `0` (for `false`) or `1` (for `true`) on the stack.
However, this leads to convoluted translation of control-flow
statements like `if` and `while`.

Example:
```c
  while (i < j) {...}
```
becomes
```scheme
  L0:            ;; beginning of loop, check condition
  iload_1        ;; i
  iload_0        ;; j
  if_icmplt L2   ;; i < j ?
  iconst_0       ;; no: put false
  goto L3
  L2:            ;; yes: put true
  iconst_1
  L3:            ;; boolean value of i < j is on top of the stack
  ifeq L1        ;; if top of stack is 0, exit while loop
  ...
  goto L0        ;; repeat loop
  L1:            ;; end of loop
```

We would like to skip the computation of the boolean value, but jump
directly according to the condition:
```scheme
  L0:            ;; beginning of loop, check condition
  iload_1        ;; i
  iload_0        ;; j
  if_icmpge L1   ;; exit loop if i ≥ j
  ...
  goto L0        ;; repeat loop
  L1:            ;; end of loop
```

We can get close to this in a compositional way by compiling boolean
expressions as jumps to either label `Ltrue` (if the expression will
get value `true`) or label `Lfalse` (otherwise).
```
compileBool (Exp e, Label Ltrue, Label Lfalse)
```

For compiling control-flow, we use it as follows:
```haskell
compileStm(SWhile e s):
  Lstart, Ltrue, Lfalse ← newLabel
  Lstart:
  compileBool (e, Ltrue, Lfalse)
  Ltrue:
  compileStm(s)
  goto Lstart
  Lfalse:

compileStm(SIfElse e s₁ s₂):
  ...
```

`compileBool` is given by the following compilation schemes:

- comparison operators:
  ```haskell
  compileBool(ELt int e₁ e₂, Ltrue, Lfalse):
    compileExp(e₁)
    compileExp(e₂)
    if_icmplt Ltrue
    goto Lfalse

  compileBool(EGt t e₁ e₂, Ltrue, Lfalse):
    ...
   ```

- logical operators:
  ```haskell
  compileBool(ETrue, Ltrue, Lfalse):
    goto ETrue

  compileBool(EFalse, Ltrue, Lfalse):
    goto EFalse

  compileBool(EAnd e₁ e₂, Ltrue, Lfalse):
    Ltrue' ← newLabel
    compileBool(e₁, Ltrue', Lfalse)
    Ltrue':
    compileBool(e₂, Ltrue, Lfalse)

  compileBool(EOr e₁ e₂, Ltrue, Lfalse):
    ...
  ```

Sometimes booleans need to be represented by 0 and 1,
e.g. when assigning to a boolean variable:
```haskell
    compileExp(e) | typeOf(e) == bool:
      Ltrue, Lfalse <- newLabel
      iconst_1                       -- speculate "true"
      compileBool(e, Ltrue, Lfalse)
      Lfalse:                        -- no? change to "false"
      pop
      iconst_0
      Ltrue:
```
