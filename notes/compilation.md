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

### Ad 1. Variable names to addresses

Jasmin types:
  * `I`: `int`
  * `D`: `double`
  * `V`: `void`
  * `Z`: `boolean`

```c
void foo                     //          size=0  max=0
  ( double x                 // x:0      size=2  max=2
  , int y) {                 // y:2      size=3  max=3
  int i;                     // i:3      size=4  max=4
  {                          // newBlock
     double y;               // y:4      size=6  max=6
     bool b;                 // b:6      size=7  max=7
  }                          // popBlock size=4  max=7
  int j;                     // j:4      size=5  max=7
}
```
```jasmin
.method public static foo(DI)V
.limit locals 7
.end method
```

### Ad 2. Expressions trees to stack instructions

Example:
```c
int main() {
  int i;                         // i:0
  double x = 3 * (i = 42) + i++; // x:1
}
```
Translates to JVM:
```scheme
.method public static main()I
.limit stack 4

;; instr   ;; expr                     ;; stack (S)   ;; store (V)
iconst_3   ;; 3                        ;; 3
bipush 42  ;; 42                       ;; 3 . 42
istore_0   ;; i =                      ;; 3           ;; 0:42
iload_0    ;; i = 42                   ;; 3 . 42
imul       ;; 3 * (i = 42)             ;; 126
iload_0    ;; i                        ;; 126 . 42
dup        ;;                          ;; 126 . 42 . 42
iconst_1   ;; 1                        ;; 126 . 42 . 42 . 1
iadd       ;; i + 1                    ;; 126 . 42 . 43
istore_0   ;; i++                      ;; 126 . 42    ;; 0:43
iadd       ;; 3 * (i = 42) + i++       ;; 168
i2d        ;; (double)                 ;; 168.0
dstore_1   ;; x = 3 * (i = 42) + i++   ;;             ;; 0:43, 1:168.0
```
Maximum stack size: 4 (32bit words)

JVM instruction small-step semantics (without jumps):
```
    I : ⟨V,S⟩ → ⟨V',S'⟩

    I      : JVM instruction (or instruction sequence)
    V / V' : variable store before/after
    S / S' : stack before/after
```

Some jump-free instructions:

|      I       |  V  |  S      |  V'      |  S'             |
|--------------|-----|---------|----------|-----------------|
| `iconst` _i_ | _V_ | _S_     | _V_      | _S.i_           |
| `bipush` _i_ | _V_ | _S_     | _V_      | _S.i_           |
| `iadd`       | _V_ | _S.i.j_ | _V_      | _S.(i+j)_       |
| `imul`       | _V_ | _S.i.j_ | _V_      | _S.(i*j)_       |
| `i2d`        | _V_ | _S.i_   | _V_      | _S.(double)i_   |
| `dup`        | _V_ | _S.i_   | _V_      | _S.i.i_         |
| `istore` _a_ | _V_ | _S.i_   | _V[a:i]_ | _S_             |
| `dstore` _a_ | _V_ | _S.d_   | _V[a:d]_ | _S_             |
| `iload` _a_  | _V_ | _S_     | _V_      | _S.V[a]_        |
| `dload` _a_  | _V_ | _S_     | _V_      | _S.V[a]_        |


Ad 3. Booleans and jumps
------------------------

Example:
```c
bool inside(int i, int j, int k) {
  return i <= j && j < k;
}
```
Machines typically do not have boolean operations; use jumps instead.
```scheme
.method public static inside(III)Z
.limit locals 3
.limit stack 2

    iload_0             ;; i
    iload_1             ;; j
    if_icmpgt Lfalse    ;; i <= j  ;; false if i > j
    iload_1             ;; j
    iload_2             ;; k
    if_icmpge Lfalse    ;; j < k   ;; false if j >= k

    iconst_1            ;; true
    goto      Ldone

Lfalse:
    iconst_0            ;; false

Ldone:
    ireturn             ;; return

.end method
```

JVM instruction small-step semantics (with jumps):
```
    I : ⟨P,V,S⟩ → ⟨P',V',S'⟩

    P / P' : code position (program counter) before/after
```

| I               | P   | V   | S       | P'    | V'  | S'  | Condition |
|-----------------|-----|-----|---------|-------|-----|-----|-----------|
| `goto` _L_      | _P_ | _V_ | _S_     | _L_   | _V_ | _S_ |           |
| `ifeq` _L_      | _P_ | _V_ | _S.i_   | _L_   | _V_ | _S_ | _i = 0_   |
| `ifeq` _L_      | _P_ | _V_ | _S.i_   | _P+1_ | _V_ | _S_ | _i ≠ 0_   |
| `ifne` _L_      | _P_ | _V_ | _S.i_   | _L_   | _V_ | _S_ | _i ≠ 0_   |
| `ifne` _L_      | _P_ | _V_ | _S.i_   | _P+1_ | _V_ | _S_ | _i = 0_   |
| `if_icmpgt` _L_ | _P_ | _V_ | _S.i.j_ | _L_   | _V_ | _S_ | _i > j_   |
| `if_icmpgt` _L_ | _P_ | _V_ | _S.i.j_ | _P+1_ | _V_ | _S_ | _i ≤ j_   |



Infrastructure of the compiler
------------------------------

Signature (global symbol table):
- name and type of functions suitable for Jasmin

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
    a <- lookupVar x
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
    emit(t-store a)              -- either istore or dstore

  compileStm(SExp t e):
    compileExp(e)
    emit(t-pop)                  -- either pop or pop2

  compileStm(SBlock ss):
    newBlock
    for (s : ss)
      compileStm(s)
    popBlock

  compileStm(SWhile e s):        -- s is a SBlock
    LStart, LEnd <- newLabel

    emit(LStart:)

    compileExp(e)                -- executing e leaves boolean on stack
    emit(ifeq LEnd)              -- if "false" end loop

    compileStm(s)                -- body of loop
    emit(goto LStart)            -- recheck loop condition

    emit(LEnd:)
```

Correctness statement (simplified):

  If         γ ⊢ s ⇓ γ'
  and        γ ~ V
  then       compileStm(s) : ⟨V,S⟩ →* ⟨V',S⟩
  such that  γ' ~ V'.

(We omit `emit` in the following.)

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
    goto LTrue

  compileBool(EFalse, Ltrue, Lfalse):
    goto LFalse

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
      Ltrue, Lfalse, Ldone <- newLabel

      compileBool(e, Ltrue, Lfalse)
      Ltrue:
      iconst_1
      goto Ldone

      Lfalse:
      pop
      iconst_0

      Ldone:
```
