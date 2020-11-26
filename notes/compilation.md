Compiling C-- to JVM
====================

Infrastructure of the compiler
------------------------------

Signature:
- name and type of functions suitable for Jasmin
- Jasmin types:
  I: int
  D: double
  V: void
  Z: boolean

Context (state):
- generate new labels
- allocate local variables; free variables when exiting a block
- keep track of maximum stack use
- append generated code (`emit`)

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

Compiling expressions
---------------------

An expression of type `t` is compiled to instructions whose effect is
to leave a new value of type `t` on top of the stack, and leave the
rest of the stack unchanged.

```
compileExp(EAdd t e₁ e₂):
  compileExp(e₁)
  compileExp(e₂)
  emit(t-add)            -- either iadd or dadd

compileExp(EVar x):
  a <- lookupVar
  emit(t-load a)         -- either iload or dload

compileExp(EAss x e):
  a <- lookupVar x
  compileExp(e)
  emit(t-store a)        -- either istore or dstore
  -- Problem here
```
(We omit `emit` in the following.)

## Compiler correctness

JVM Small-step semantics without jumps:

  i : ⟨V,S⟩ → ⟨V',S'⟩

  i      : JVM instruction (or instruction sequence)
  V / V' : variable store before/after
  S / S' : stack before/after

We say  γ ~ V  if environment γ translates to variable store V.

Correctness statement (simplified):

  If         γ ⊢ e ⇓ ⟨v, γ'⟩
  and        γ ~ V
  then       compileExp(e) : ⟨V,S⟩ →* ⟨V',S.v⟩
  such that  γ' ~ V'.

Compiling statements
--------------------

A statement (sequence) is compiled to instructions whose effect on the
stack is nil (no change).

```
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
```jasmin
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
```jasmin
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
```
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
  ```
  compileBool(ELt int e₁ e₂, Ltrue, Lfalse):
    compileExp(e₁)
    compileExp(e₂)
    if_icmplt Ltrue
    goto Lfalse

  compileBool(EGt t e₁ e₂, Ltrue, Lfalse):
    ...
   ```

- logical operators:
  ```
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
```
compileExp(e) | typeOf(e) == bool:
  Ltrue, Lfalse <- newLabel
  iconst_1                       -- speculate "true"
  compileBool(e, Ltrue, Lfalse)
  Lfalse:                        -- no? change to "false"
  pop
  iconst_0
  Ltrue:
```
