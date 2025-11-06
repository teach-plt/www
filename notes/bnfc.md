---
title: BNFC syntax
subtitle: Programming Language Technology, DAT151/DIT231
---

# BNFC rule format

The basic format is of a rule is:

    LABEL "." CATEGORY "::=" (CATEGORY | TERMINAL)* ";"

For example:

    DFun.  Def ::= Type Id "(" [Decl] ")" Body ;

Herein `Def`, `Type`, `Id`, `Body` are ordinary categories.
`[Decl]` is a list category.
`"("` and `")"` are terminals.

Categories can be defined by rules or `token` definitions (see below).

# BNFC macros

## Coercions

Standard embedding of higher precedence levels into lower ones can be abbreviated with the `coercions` macro:

    coercions Exp 3;

expands to

    _. Exp3 ::= "(" Exp ")";
    _. Exp2 ::= Exp3;
    _. Exp1 ::= Exp2;
    _. Exp  ::= Exp1;

Common mistake in exam:
if you use `coercions`, you already get a rule for parenthesized expressions,
so do not add

    EPar.  Exp3 ::= "(" Exp ")";

This will create a reduce/reduce conflict.

## List macros

The list macros `terminator` and `separator` generate list categories.

Example: terminating each list entry with non-terminal `";"`:

    terminator Line ";" ;

expands to

    [].  [Line] ::= ;
    (:). [Line] ::= Line ";" [Line];

Example: separating list entries with non-terminal ",", requiring at least one element:

    separator nonempty Exp "," ;

expands to

    (:[]).  [Exp] ::= Exp ;
    (:).    [Exp] ::= Exp "," [Exp] ;

Example: no separator.  Both

    terminator Stm "" ;
    separator  Stm "" ;

expand to

    [].  [Stm] ::= ;
    (:). [Stm] ::= Stm [Stm];

Common mistake in exam:
List categories are not defined automatically (even BNFC could do this, defaulting to no separator).
You need to either use a list macro or define them by hand.
(In the exam, you might be asked to define them by hand rather than by a macro.)

## Comment macros

There are short hands for lexing comments:

- Non-nested block comments can be defined by e.g. `comment "/*" "*/";` (binary form of `comment` pragma).
- Line comments can be defined by e.g. `comment "//";` (unary form of `comment` pragma).

The `comment` pragma generates regular expressions for the lexer that produce no token (just like whitespace produces no token).

# BNFC token definitions

BNFC has some built-in token definitions:

| category  | meaning                          | example(s)                              |
|-----------|----------------------------------|-----------------------------------------|
| `Ident`   | **Haskell**-style identifiers    | `int_var6'`                             |
| `Integer` | unsigned integer literals        | `123`, `0`, `007`                       |
| `Double`  | unsigned floating-point literals | `3.14159263`                            |
| `String`  | Haskell string literals          | `"He said \"hello \\ (backslash)\" \n"` |
| `Char`    | Haskell character literals       | `'a'` , `'\''`, `'\n'`, `'\t'`          |

With the `token` pragma we can define our own tokens through regular expressions.

Example: C++ identifiers

    token Id letter (letter | digit | '_')* ;

Example: simple floating point literals

    token Float digit* ('.' digit+)? ;

Example: any non whitespace character

    token Char1 char - [" \t\n"] ;

Example: hexadecimal number

    token Hex '0' 'x' (digit | ["abcdefABCDEF"])+ ;

See:

- https://bnfc.readthedocs.io/en/latest/lbnf.html#the-token-rule
- https://bnfc.readthedocs.io/en/latest/lbnf.html#the-lexical-structure-of-bnf

The documentation sweeps under the carpet that _difference_ _r₁_ `-` _r₂_
is only implemented for character classes, so here is a more precise grammar
of regular expressions to be used in `token` pragmas:

- Character classes _c_ denote sets of characters.

  | character class _c_ | meaning                         |
  |---------------------|---------------------------------|
  | `'a'`               | exactly character `'a'`         |
  | `digit`             | `'0' \| ... \| '9'`             |
  | `letter`            | latin letter                    |
  | `lower`             | lower case latin letter         |
  | `upper`             | upper case latin letter         |
  | `char`              | any character                   |
  | _c₁_ `-` _c₂_       | exclusion: _c₁_ but not _c₂_    |
  | _c₁_ `\|` _c₂_      | alternative: in _c₁_ or _c₂_    |
  | `["ABC"]`           | alternative `'A' \| 'B' \| 'C'` |

- Regular expressions _r_ denote sets of words (character sequences).

  | regex          | meaning                                             |
  |----------------|-----------------------------------------------------|
  | _c_            | single character from class _c_                     |
  | _r₁_ `\|` _r₂_ | alternative: _r₁_ or _r₂_                           |
  | _r₁ r₂_        | sequence: _r₁_ followed by _r₂_                     |
  | `eps`          | empty sequence                                      |
  | `{"abc"}`      | sequence `'a' 'b' 'c'`                              |
  | _r_ `?`        | optional: zero or one repetitions of _r_            |
  | _r_ `*`        | iteration: zero or more repetitions of _r_          |
  | _r_ `+`        | non-empty iteration: one or more repetitions of _r_ |
