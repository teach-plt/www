PLT 2024-12-19: Walk through exam
=================================

Q1
--

```
-- Q1: Grammar for some C constructs

-- Question 1 (Grammars): Write a labelled BNF grammar that covers the following kinds of constructs of C:

-- • Program: int main() followed by a block

Prg. Program ::= "int" "main" "(" ")" Block;

-- • Block: a sequence of statements enclosed between { and }

Blk. Block   ::= "{" [Stm] "}"

[].  [Stm]   ::= ;
(:). [Stm]   ::= Stm [Stm];

-- • Statement:
-- – statement formed from an expression by adding a semicolon ;

SExp. Stm ::= Exp ";" ;

-- – initializing variable declarations, e.g., int x = e;

SInit. Stm ::= Type Ident "=" Exp ";" ;

-- – assignment, e.g., x = e;

SAss.  Stm ::= Ident "=" Exp ;

-- – loop: while followed by a parenthesized expression and a block

SWhile. Stm ::= "while" "(" Exp ")" Block;

-- • Atomic expression:
-- – identifier

EId.   Exp2 ::= Ident ;

-- – integer literal

EInt.  Exp2 ::= Integer;

-- – function call with a single argument

ECall. Exp2 ::= Ident "(" Exp ")" ;

-- – pre-increment of identifier, e.g., ++x

EInc.  Exp2 ::= "++" Ident;

-- • Expression (from highest to lowest precedence):
-- – atomic expression
-- – addition (+), left-assocative

EAdd.  Exp1 ::= Exp1 "+" Exp2;

-- – less-than comparison of integer expressions (<), non-associative

ELt.   Exp  ::= Exp1 "<" Exp1;

-- coercions Exp 2;

_.   Exp ::= Exp1;
_.   Exp1 ::= Exp2;

-- – parenthesized expression
_.   Exp2 ::= "(" Exp ")";

-- • Type: int or bool

TInt.  Type ::= "int";
TBool. Type ::= "bool";

-- Lines starting with # are comments.

comment "#";

-- You can use the standard BNFC categories Integer and Ident and the coercions pragma.
-- Do not use list categories via the terminator and separator pragmas!
```

Q2
--

DFA: States 1(initial) 2 3 4 5(final)
```
1 --S--> 2
2 --A--> 3
3 --c--> 3
3 --S--> 3
3 --A--> 4
4 --A--> 4
4 --c--> 3
4 --S--> 5
```

RE: `SA(c+S)*AA*(c(c+S)*AA*)*S`

Q3
--

```
Start. S ::= M P ;
MEmp.  M ::= ;
MBin.  M ::= M A "*";
PEmp.  P ::= ;
PBin.  P ::= A "+" P ;
X.     A ::= "x" ;
Y.     A ::= "y" ;
```
Step by step, trace the shift-reduce parsing of the expression
```
x*y*y+x+
```


| Stack       | Input      | Action         |
|-------------|------------|----------------|
|             | `x*y*y+x+` | Reduce w MEmp  |
| M           | `x*y*y+x+` | Shift          |
| M x         |  `*y*y+x+` | Reduce w X     |
| M A         |  `*y*y+x+` | Shift          |
| M A *       |   `y*y+x+` | Reduce w MBin  |
| M y         |    `*y+x+` | Reduce w Y     |
| M A         |     `y+x+` | Shift          |
| M A *       |     `y+x+` | Reduce w MBin  |
| M           |     `y+x+` | Shift          |
| M y         |      `+x+` | Reduce w Y     |
| M A         |      `+x+` | Shift          |
| M A +       |       `x+` | Shift          |
| M A + x     |        `+` | Reduce w X     |
| M A + A     |        `+` | Shift          |
| M A + A +   |            | Reduce w PEmp  |
| M A + A + P |            | Reduce w PBin  |
| M A + P     |            | Reduce w PBin  |
| M P         |            | Reduce w Start |
| S           |            | Accept         |


Q4
--

### Checking statements

```
SExp.   Stm   ::= Exp ";" ;
SInit.  Stm   ::= Type Ident "=" Exp ";" ;
SAss.   Stm   ::= Ident "=" Exp ;
SWhile. Stm   ::= "while" "(" Exp ")" Block;

Blk.    Block ::= "{" [Stm] "}"
[].     [Stm] ::= ;
(:).    [Stm] ::= Stm [Stm];
```

```
Env: stack of blocks each mapping variables names to types

-- mutually defined:
Env check (Env env, Stm s)
Env check (Env env, Block b)
Env check (Env env, [Stm] ss)

-- assuming
Type inferExp (Env env, Exp e)
void checkExp (Env env, Exp e, Type t)

-- cases

check(env, SExp e) = do
  t ← inferExp(env, e)
  return env

check(env, SInit t x e) = do
  if x is in top block of env then error
  env1 ← addVar(env, t, x)
  checkExp(env1, e, t)
  return env1

check(env, SAss x e) = do
  t ← lookupVar (env, x)  -- throws error if x is not in env
  checkExp(env, e, t)
  return env

check(env, SWhile e b) = do
  checkExp(env, e, TBool)
  checkBlock(env, b)
  return env

check(env, SBlock ss) = do
  env1 ← newBlock(env)
  check(env1, ss)
  return env

check(env, []) = return env
check(env, s:ss) = do
  env1 ← check(env, s)
  env2 ← check(env1, ss)
  return env2
```

### Evaluating expressions

```
EId.   Exp2 ::= Ident ;
EInt.  Exp2 ::= Integer;
ECall. Exp2 ::= Ident "(" Exp ")" ;  -- omit
EInc.  Exp2 ::= "++" Ident;
EAdd.  Exp1 ::= Exp1 "+" Exp2;
ELt.   Exp  ::= Exp1 "<" Exp1;
```

```
Env: maps identifiers to values

Val: either VInt (with an integer)
  or VBool (with a boolean)

-- assuming
Val lookupVar (Env env, Ident x)

(Env, Val) eval (Env env, Exp e)

eval(env, EId x) = do
  v ← lookupVar(env, x)
  return (env, v)

eval(env, EInt i) = return (env, VInt i)

eval(env, EInc x) = do
  VInt i ← lookupVar(env, x)
  env1   ← updateVar(env, x, i+1)
  return (env1, VInt (i+1))

eval(env, EAdd e1 e2) = do
  (env1, VInt i1) ← eval(env, e1)
  (env2, VInt i2) ← eval(env1, e2)
  return (env2, VInt (i1 + i2))

eval(env, ELt e1 e2) = do
  (env1, VInt i1) ← eval(env, e1)
  (env2, VInt i2) ← eval(env1, e2)
  return (env2, VBool (i1 < i2))
```

Q5
--

```
.method public static int main()
.limit locals 3
.limit stack

;; int n=42;

    ldc 42
    istore 0

;; int i=0;

    ldc 0
    istore 1

;; int k=0;

    ldc 0
    istore 2

Lstart:
;; while(k<101)

    iload 2
    ldc   101
    if_icmpge Ldone

;; { n = k;

    iload 2
    istore 0

;;   k = n + ++i; }

    iload 0
    iinc  1 1
    iload 1
    iadd
    istore 2

    goto Lstart

Ldone:
;;  printInt(n);

    iload 0
    invokestatic Runtime/printInt(I)V

    ldc 0
    ireturn

.end method
```

| Instruction      | P | V   | S         | P'  | V'       | S'        | Condition  |
|------------------|---|-----|-----------|-----|----------|-----------|------------|
| `ldc` i          | P | V   | S         | P+1 | V        | S.i       |            |
| `istore` a       | P | V   | S.i       | P+1 | V[a=i]   | S         |            |
| `iload` a        | P | V   | S         | P+1 | V        | S.V[a]    |            |
| `iinc` a i       | P | V   | S         | P+1 | V[a=v+i] | S         | v=V[a]     |
| `iadd`           | P | V   | S.i1.i2   | P+1 | V        | S.(i1+i2) |            |
| `goto` L         | P | V   | S         | L   | V        | S         |            |
| `if_icmpge` L    | P | V   | S.i1.i2   | P+1 | V        | S         | if i1 < i2 |
| `if_icmpge` L    | P | V   | S.i1.i2   | L   | V        | S         | if i1 ≥ i2 |
| `invokestatic` m | P | V   | S.v       | P+1 | V        | S.m(v)    | m unary    |
