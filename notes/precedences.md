---
title: Quiz: Precedences in LBNF
subtitle: Programming Language Technology, DAT151/DIT231
---

Quiz: https://menti.com Code 77963396
(URL: https://menti.com/alrguvges59f)

Binary operators
----------------

Assume:
```
coercions Exp 1;
```

For the language `Exp` of divisions, which is correct?
(Pick one or more alternatives.)

1. ```
   EInt. Exp  ::= Integer;
   EDiv. Exp1 ::= Exp1 "/" Exp;
   ```

2. ```
   EInt. Exp1 ::= Integer;
   EDiv. Exp  ::= Exp1 "/" Exp;
   ```

3. ```
   EInt. Exp1 ::= Integer;
   EDiv. Exp  ::= Exp "/" Exp1;
   ```

Quiz: https://menti.com Code 77963396

Prefix and postfix operators
----------------------------

The language `Exp` shall have pre-increment and array indexing,
the latter binding stronger than pre-increment.
Which encoding is correct?

1. ```
   EId.   Exp2 ::= Ident;
   EArr.  Exp1 ::= Exp2 "[" Exp1 "]";
   EInc.  Exp  ::= "++" Exp1;
   coercions Exp 2;
   ```

2. ```
   EId.   Exp1 ::= Ident;
   EArr.  Exp1 ::= Exp1 "[" Exp1 "]";
   EInc.  Exp  ::= "++" Exp1;
   coercions Exp 1;
   ```

3. ```
   EId.   Exp1 ::= Ident;
   EArr.  Exp1 ::= Exp1 "[" Exp "]";
   EInc.  Exp ::= "++" Exp;
   coercions Exp 1;
   ```

4. ```
   EId.   Exp1 ::= Ident;
   EArr.  Exp1 ::= Exp1 "[" Exp1 "]";
   EInc.  Exp ::= "++" Exp;
   coercions Exp 1;
   ```

Hint: Parse `++x[i[j]][++k]`.


Mixfix operators
----------------

The conditional `_?_:_` is a right associative operator of level 2.
Which is the correct rule?

1. `ECond.  Exp  ::= Exp2 "?" Exp2 ":" Exp2;`
2. `ECond.  Exp  ::= Exp2 "?" Exp  ":" Exp ;`
3. `ECond.  Exp2 ::= Exp2 "?" Exp  ":" Exp ;`
4. `ECond.  Exp2 ::= Exp2 "?" Exp2 ":" Exp ;`
5. `ECond.  Exp2 ::= Exp2 "?" Exp2 ":" Exp2;`
6. `ECond.  Exp2 ::= Exp2 "?" Exp2 ":" Exp3;`
7. `ECond.  Exp2 ::= Exp3 "?" Exp" ":" Exp2;`
8. `ECond.  Exp2 ::= Exp3 "?" Exp2 ":" Exp2;`
9. `ECond.  Exp2 ::= Exp3 "?" Exp3 ":" Exp3;`

`coercions Exp 15`
`e1 ? e2 : e3 ? e4 : e5`
