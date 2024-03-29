---
title: Example parse table for shift/reduce parsing
subtitle: Programming Language Technology, DAT151/DIT231
---



Grammar
-------

    EProd.  Exp     ::= Exp1;
    EAdd.   Exp     ::= Exp "+" Exp1;
    EInt.   Exp1    ::= Integer;
    EMul.   Exp1    ::= Exp1 "*" Integer;
    LInt.   Integer ::= int;

    %start. S       ::= Exp eof;

Generated parse table (in Happy-info layout)
--------------------------------------------

State 0

        int         shift to state 1

        Exp         goto state 2
        Exp1        goto state 3
        Integer     goto state 4


State 1

        eof         reduce with rule LInt
        '*'         reduce with rule LInt
        '+'         reduce with rule LInt

State 2

        eof         reduce with rule %start
        '+'         shift to state 5

State 3

        eof         reduce with rule EProd
        '*'         shift to state 7
        '+'         reduce with rule EProd

State 4

        eof         reduce with rule EInt
        '*'         reduce with rule EInt
        '+'         reduce with rule EInt

State 5

        int         shift to state 1

        Exp1        goto state 6
        Integer     goto state 4


State 6

        eof         reduce with rule EAdd
        '*'         shift to state 7
        '+'         reduce with rule EAdd

State 7

        int         shift to state 1

        Integer     goto state 8


State 8

        eof         reduce with rule EMul
        '*'         reduce with rule EMul
        '+'         reduce with rule EMul


States
------

State 0

    S       → . Exp eof
    Exp     → . Exp "*" Exp1
    Exp     → . Exp1
    Exp1    → . Exp1 "*" Integer
    Exp1    → . Integer
    Integer → . int


| State | Parse item 1                | Parse item 2                |
|-------|-----------------------------|-----------------------------|
| 0     | `S    → . Exp eof`          |                             |
| 1     | `Integer → int .`           |                             |
| 2     | `S    → Exp . eof`          | `Exp  → Exp . "+" Exp1`     |
| 3     | `Exp  → Exp1 .`             | `Exp1 → Exp1 . "*" Integer` |
| 4     | `Exp1 → Integer .`          |                             |
| 5     | `Exp  → Exp "+" . Exp1`     |                             |
| 6     | `Exp  → Exp "+" Exp1 .`     | `Exp1 → Exp1 . "*" Integer` |
| 7     | `Exp1 → Exp1 "*" . Integer` |                             |
| 8     | `Exp1 → Exp1 "*" Integer .` |                             |


Parse table
-----------

| State |  eof    |   `+`   |   `*`   | `int` | `Exp` | `Exp1` |`Integer`|
|-------|---------|---------|---------|-------|-------|--------|---------|
|   0   |         |         |         |  s1   |  g2   |   g3   |   g4    |
|   1   | `LInt`  | `LInt`  | `LInt`  |       |       |        |         |
|   2   | `%start`|  s5     |         |       |       |        |         |
|   3   | `EProd` | `EProd` |  s7     |       |       |        |         |
|   4   | `EInt`  | `EInt`  | `EInt`  |       |       |        |         |
|   5   |         |         |         |  s1   |       |   g6   |   g4    |
|   6   | `EAdd`  | `EAdd`  |  s7     |       |       |        |         |
|   7   |         |         |         |  s1   |       |        |   g8    |
|   8   | `EMul`  | `EMul`  | `EMul`  |       |       |        |         |

Parser run on 1+2*3
-------------------

    ε                        . '1' '+' '2' '*' '3' -- shift
    int(1)                   . '+' '2' '*' '3'     -- reduce with rule LInt
    Integer                  . '+' '2' '*' '3'     -- reduce with rule EInt
    Exp1                     . '+' '2' '*' '3'     -- reduce with rule EProd
    Exp                      . '+' '2' '*' '3'     -- shift
    Exp '+'                  . '2' '*' '3'         -- shift
    Exp '+' int(2)           . '*' '3'             -- reduce with rule LInt
    Exp '+' Integer          . '*' '3'             -- reduce with rule EInt
    Exp '+' Exp1             . '*' '3'             -- shift
    Exp '+' Exp1 '*'         . '3'                 -- shift
    Exp '+' Exp1 '*' int(3)  .                     -- reduce with rule LInt
    Exp '+' Exp1 '*' Integer .                     -- reduce with rule EMul
    Exp '+' Exp1             .                     -- reduce with rule EAdd
    Exp                      .                     -- halt
