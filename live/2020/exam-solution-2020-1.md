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



Q5: Compilation
===============



Q6: Functional languages
========================



End.
