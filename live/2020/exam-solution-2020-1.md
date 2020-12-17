---
title: Programming Language Technology DAT151/DIT231
date: 2020-12-17 Training exam
author: Andreas Abel
abstract: We go through the exam of 2020-01-13.
---


Q1: Grammars
============

see Exam.cf


Q2: Lexing
==========

1. Regular expression for acceptable password

Syntax elements : .(Σ) a b c |  * + ( ) and juxtaposition for sequence

Possible solutions:

  - Σ* a Σ* b Σ* | Σ* b Σ* a Σ*
  - Σ* (a Σ* b | b Σ* a) Σ*

2. Automaton for acceptable password

States:

  - 0   haven't seen a or b yet  (initial state)
  - a   seen a but not b
  - b   seen b but not a
  - ab  seen both a and b        (final state)

Transition table:

  - columns: states
  - rows: possible next character

|   | 0 | a  | b  | ab |
|---|---|----|----|----|
| a | a | a  | ab | ab |
| b | b | ab | b  | ab |
| c | 0 | a  | b  | ab |



Q3: LR parsing
==============

Notation: stack . remaining input // action

            . x + y - x // reduce with None
    P       . x + y - x // shift x
    P x     . + y - x   // reduce with X
    P A     . + y - x   // shift +
    P A +   . y - x     // reduce with Plus
    P       . y - x     // shift y
    P y     . - x       // reduce with Y
    P A     . - x       // shift -
    P A -   . x         // shift x
    P A - x .           // reduce with X
    P A - A .           // reduce with Done
    P A - M .           // reduce with Minus
    P M     .           // reduce with Seq
    S       .           // accept


Q4: Type checking and evaluation
================================

1. Type checking statements

   - Γ ⊢ s ⇒ Γ' : check statement s in context Γ, returning updated context Γ'
   - Γ ⊢ ss ⇒ Γ': check statement sequence ss in context Γ, returning updated context Γ'

   - Γ: non-empty stack of blocks Δ₁.Δ₂. ... .Δₙ
   - Δ: finite map from variable identifiers x to types t:  x₁:t₁,...,xₘ:tₘ

   - Rules

        Γ. ⊢ ss ⇒ Γ.Δ
        ------------------------
        Γ ⊢ SBlock (Blk ss) ⇒ Γ


        Γ.(Δ,x:t) ⊢ e : t
        ------------------------------ x ∉ Δ
        Γ.Δ ⊢ SInit t x e ⇒ Γ.(Δ,x:t)


        Γ ⊢ e : t
        ---------------
        Γ ⊢ SExp e ⇒ Γ


        Γ ⊢ e : TBool
        Γ. ⊢ s₁ ⇒ Γ.Δ
        Γ. ⊢ s₂ ⇒ Γ.Δ
        ------------------------
        Γ ⊢ SIfElse e s₁ s₂ ⇒ Γ


2. Interpretation of expressions

  - γ ⊢ e ⇓ ⟨v;γ'⟩

  - γ: non-empty list of blocks δ: γ₁. ... .γₙ
  - δ: finite map from variables x to value v:  x₁=v₁,...,xₘ=vₘ

  - Values v are either boolean (`true`, `false`) or integer (i) literals

  - Rules

        --------------------
        γ ⊢ ETrue ⇓ ⟨true;γ⟩

        ----------------------
        γ ⊢ EFalse ⇓ ⟨false;γ⟩

        ------------------
        γ ⊢ EInt i ⇓ ⟨i;γ⟩

        γ ⊢ e₁ ⇓ ⟨v₁;γ₁⟩
        γ₁ ⊢ e₂ ⇓ ⟨v₂;γ₂⟩
        ...
        γ(n-1) ⊢ eₙ ⇓ ⟨vₙ;γₙ⟩
        x₁=v₁,...,xₙ=vₙ ⊢ b ⇓ v
        ------------------------------------ t f(t₁ x₁, ..., tₙ xₙ) b
        γ ⊢ ECall f [e₁,...,eₙ] ⇓ ⟨v;γₙ⟩

        ------------------------ i = γ(x)   γ' = γ[x=i+1]
        γ ⊢ EPostIncr x ⇓ ⟨i;γ'⟩

        γ ⊢ e₁ ⇓ ⟨i₁;γ₁⟩
        γ₁ ⊢ e₂ ⇓ ⟨i₂;γ₂⟩
        ----------------------------
        γ ⊢ EPlus e₁ e₂ ⇓ ⟨i₁+i₂;γ₂⟩




Q5: Compilation
===============

Compilation state:
  - stack `Γ` of blocks, each being a finite map from variable identifiers `x` to types `t` and addresses `a`
  - output: list of JVM instructions
  - potentially infinite supply of fresh labels

Service functions:
  - `newBlock`: push an empty block on the stack `Γ`
  - `popBlock`: remove top block from `Γ`
  - `addVar t x`: put a new entry `x ↦ (t,a)` in top block of Γ, `a` being the next free address
  - `lookupVar x`: lookup address `a` of `x` in `Γ`
  - `emit i`: append JVM instruction `i` to the output
  - `newLabel`: return the next label from the label supply
  - `lookupFun f`: return the JVM name of function `f`

Part 1. Compilation schemes:

    compile(Blk ss):
      newBlock
      for (s : ss)
        compile(s)
      popBlock

    compile(SBlock b):
      compile(b)

    compile(SInit t x e):
      a ← addVar t x
      compile(e)
      emit (istore a)

    compile(SExp e):
      compile(e)
      emit(pop)

    compile(SIfElse e s₁ s₂):
      Lelse ← newLabel
      Ldone ← newLabel
      compile(e)
      emit(ifeq Lelse)
      newBlock
      compile(s₁)
      popBlock
      emit(goto Ldone)
      emit(Lelse:)
      newBlock
      compile(s₂)
      popBlock
      emit(Ldone:)

    compile(ETrue):
      emit(ldc 1)

    compile(EFalse):
      emit(ldc 0)

    compile(EInt i):
      emit(ldc i)

    compile(ECall f es):
      g ← lookupFun f
      for (e : es)
        compile(e)
      emit(invokestatic g)

    compile(EPostIncr x):
      a ← lookupVar x
      emit(iload a)
      emit(dup)
      emit(ldc 1)
      emit(iadd)
      emit(istore a)

    compile(EPlus e₁ e₂):
      compile(e₁)
      compile(e₂)
      emit(iadd)

Part 2. Small-step semantics

    iadd           : (P,V,S.i.j)      → (P+1,V,     S.(i+j))
    istore a       : (P,V,S.i)        → (P+1,V[a=i],S)
    ldc i          : (P,V,S)          → (P+1,V,     S.i)
    dup            : (P,V,S.i)        → (P+1,V,     S.i.i)
    pop            : (P,V,S.i)        → (P+1,V,     S)
    iload  a       : (P,V,S)          → (P+1,V,     S.V[a])
    invokestatic g : (P,V,S.i₁....iₙ) → (P+1,V,     S.i)
      g is a method expecting n int arguments and returning and int
      i should be the value computed by g from arguments i₁...iₙ
    ifeq L         : (P,V,S.i)        → (L,  V,     S)  if i=0
    ifeq L         : (P,V,S.i)        → (P+1,V,     S)  if i≠0
    goto L         : (P,V,S)          → (L,  V,     S)



Q6: Functional languages
========================

1. a) no
   b) no
   c) yes
   d) yes
   e) yes   (modulo extra parenthesis)

2. Call-by-name interpreter

See lecture notes

Environment `γ` maps variables `x` to closures `⟨e;γ⟩`
Values are either integer literals `n` or function closures `⟨λx→e;γ⟩`

Judgement: `γ ⊢ e ⇓ v`

Rules:

    δ ⊢ e ⇓ v
    ----------- γ(x) = ⟨e;δ⟩
    γ ⊢ x ⇓ v

    -------------------
    γ ⊢ λx→e ⇓ ⟨λx→e;γ⟩

    γ ⊢ e₁ ⇓ ⟨λx→e;δ⟩
    δ,x=⟨e₂;γ⟩ ⊢ e ⇓ v
    -------------------
    γ ⊢ e₁ e₂ ⇓ v

    ----------
    γ ⊢ n ⇓ n

    γ ⊢ e₁ ⇓ n₁
    γ ⊢ e₂ ⇓ n₂
    ---------------------
    γ ⊢ e₁ + e₂ ⇓ n₁ + n₂


End.
