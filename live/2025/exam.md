Question 1 (Grammars)
=====================

Write a labelled BNF grammar that covers the following
kinds of constructs of C/C++ (sublanguage of lab 2):
• Program: a sequence of function definitions.
• Function definition: type, identifier, comma-separated parameter list in parenthe-
ses, block.
• Parameter: type followed by identifier, e.g. int x.
• Block: a sequence of statements enclosed between { and }
• Statements:
– block
– initializing variable declaration, e.g., int x = 5;
– return statement
– if-else statement
• Expressions, from highest to lowest precedence:
– parenthesized expression, integer literal (e.g. 42), identifier (e.g. x)
– less-or-equal-than comparison (<=), non-associative
– short-circuiting disjunction (||), left associative
– assignment to identifiers (x = e), right associative
• Type: int or bool

Solution
--------

```
-- Q1

-- • Program: a sequence of function definitions.

Prog. Program ::= [Def];

-- • Function definition: type, identifier, comma-separated parameter list in parenthe-
-- ses, block.

DFun. Def ::= Type Ident [Parameter] Block;
separator Def "";

-- • Parameter: type followed by identifier, e.g. int x.

PDecl. Parameter ::= Type Ident;
separator Parameter ",";

-- • Block: a sequence of statements enclosed between { and }

BBlock. Block ::= "{" [Stm] "}";

-- • Statements:
-- – block

SBlock.  Stm ::= Block;

-- – initializing variable declaration, e.g., int x = 5;

SInit. Stm ::= Type Ident "=" Exp ";";

-- – return statement

SReturn.  Stm ::= "return" Exp ";";

-- – if-else statement

SIfElse.  Stm ::= "if" "(" Exp ")" Stm "else" Stm;

separator Stm "";

-- • Expressions, from highest to lowest precedence:
-- – parenthesized expression, integer literal (e.g. 42), identifier (e.g. x)

EInt.  Exp3 ::= Integer;
EId.   Exp3 ::= Ident;

-- – less-or-equal-than comparison (<=), non-associative

ELEq.  Exp2 ::= Exp3 "<=" Exp3;

-- – short-circuiting disjunction (||), left associative

EOr.   Exp1 ::= Exp1 "||" Exp2;

-- – assignment to identifiers (x = e), right associative

EAss.  Exp ::= Ident "=" Exp;

coercions Exp 3;

-- • Type: int or bool

TInt.  Type ::= "int";
TBool. Type ::= "bool";
```


Question 2 (Lexing)
===================

An identifier be a non-empty sequence of letters and under-
scores, with the following limitations:

• Underscores cannot follow immediately after another.
• An identifier cannot be just an underscore.

Letters be subsumed under the non-terminal a; besides letters, we only consider under-
scores u; thus, the alphabet is just {a,u}.

1. Give a regular expression for identifiers.
2. Give a deterministic finite automaton for identifiers with no more than 7 states.

Remember to mark initial and final states appropriately. (4p)

Solution
--------

1. RE:  `u? (a+ u)* a+ u?`
   ALT: `u? (a+ u?)+`

2. Automaton (terrible ASCII art)


                        a
                 a     ( )    u
          --> s0 ---> ((s2)) ---> ((s3))
              |    /         <---
              |u  /a           a
              v  /
              s1



Question 3 (LR Parsing)
=======================

Consider the following labeled BNF-Grammar.

    Cons. L ::= L "," L ;
    Foo. L ::= "foo" ;
    Bar. L ::= "bar" ;
    Doh. L ::= "doh" ;

We work with the following example string:

    foo , bar , doh

1. The grammar is ambiguous. Show this by giving two different parse trees for the
example string.

         Cons Foo (Cons Bar Doh)
         Cons (Cons Foo Bar) Doh

2. The LR parser generated for this grammar has a shift-reduce conflict.
Step by step, trace the parsing of the example string showing how the stack and
the input evolve and which actions are performed. Resolve any shift-reduce conflict
in favor of shift.

                     . foo , bar , doh -- shift
         foo         .     , bar , doh -- reduce with Foo
         L           .     , bar , doh -- shift 2x
         L , bar     .           , doh -- reduce with Bar
         L , L       .           , doh -- shift 2x         (!!)
         L , L , doh .                 -- reduce with Doh
         L , L , L   .                 -- reduce with Cons
         L , L       .                 -- reduce with Cons
         L           .                 -- accept

Which of the two parse trees is produced by this run?

         Cons Foo (Cons Bar Doh)

3. Now do the trace again, this time resolving conflicts in favor of reduce.

                     . foo , bar , doh -- shift
         foo         .     , bar , doh -- reduce with Foo
         L           .     , bar , doh -- shift 2x
         L , bar     .           , doh -- reduce with Bar
         L , L       .           , doh -- reduce with Cons
         L           .           , doh -- shift 2x
         L , doh     .                 -- reduce with Doh, Cons
         L                             -- accept

Which of the two parse trees is produced by that run?

         Cons (Cons Foo Bar) Doh

(8p)


Version with non-terminals annotated with the parse tree:

                                    . foo , bar , doh -- shift
         foo                        .     , bar , doh -- reduce with Foo
         L(Foo)                     .     , bar , doh -- shift 2x
         L(Foo) , bar               .           , doh -- reduce with Bar
         L(Foo) , L(Bar)            .           , doh -- shift 2x         (!!)
         L(Foo) , L(Bar) , doh      .                 -- reduce with Doh
         L(Foo) , L(Bar) , L(Doh)   .                 -- reduce with Cons
         L(Foo) , L(Cons Bar Doh)   .                 -- reduce with Cons
         L(Cons Foo (Cons Bar Doh)) .                 -- accept


Question 4 (Type checking and evaluation)
=========================================

1. Write syntax-directed type checking rules for the statement forms and blocks of
Question 1. The form of the typing judgements should be Γ ⊢t s⇒Γ′ where sis a
statement or list of statements, t the return type, Γ is the typing context before s,
and Γ′ the typing context after s. Observe the scoping rules for variables! You can
assume a type-checking judgement Γ ⊢e: t for expressions e.
Alternatively, you can write the type checker in pseudo code or Haskell (then assume
checkExpr to be defined). In any case, the typing context and the return type must
be made explicit. (6p)


        Γ.ε ⊢ₜ ss ⇒ Γ'
        ------------------
        Γ ⊢ₜ SBlock ss ⇒ Γ

        ----------
        Γ ⊢ₜ [] ⇒ Γ

        Γ  ⊢ₜ s ⇒ Γ₁
        Γ₁ ⊢ₜ ss ⇒ Γ₂
        ----------------
        Γ ⊢ₜ (s:ss) ⇒ Γ₂

        Γ.(Δ,x:t) ⊢ e : t
        ------------------------------ x ∉ Δ
        Γ.Δ ⊢ₜ SInit t x e ⇒ Γ.(Δ,x:t)

        Γ ⊢ e : t
        ------------------
        Γ ⊢ₜ SReturn e ⇒ Γ

        Γ ⊢ e : bool   Γ.ε ⊢ₜ s1 ⇒ Γ₁   Γ.ε ⊢ₜ s2 ⇒ Γ₂
        ---------------------------------------------
        Γ ⊢ₜ IfElse e s1 s2 ⇒ Γ


2. Write syntax-directed interpretation rules for the expressions of Question 1. The
form of the evaluation judgement should be γ ⊢e ⇓⟨v; γ′⟩where e denotes the
expression to be evaluated in environment γand the pair ⟨v; γ′⟩denotes the resulting
value and updated environment.
Alternatively, you can write the interpreter in pseudo code or Haskell. Functions
lookupVar and updateVar can be assumed if their behavior is described. In any
case, the environment must be made explicit. (6p)

        -------------------
        γ ⊢ EInt i ⇓ ⟨i; γ⟩

        ------------------- γ(x) = v
        γ ⊢ EId x  ⇓ ⟨v; γ⟩

        γ ⊢ e1 ⇓ ⟨i1; γ₁⟩  γ₁ ⊢ e2 ⇓ ⟨i2; γ₂⟩
        -------------------------------------
        γ ⊢ ELEq e1 e2 ⇓ ⟨ i1≤i2; γ₂⟩

        γ ⊢ e1 ⇓ ⟨true; γ₁⟩
        ---------------------------
        γ ⊢ EOr e1 e2 ⇓  ⟨true; γ₁⟩

        γ ⊢ e1 ⇓ ⟨false; γ₁⟩  γ₁ ⊢ e2 ⇓ ⟨b; γ₂⟩
        ---------------------------------------
        γ ⊢ EOr e1 e2 ⇓ ⟨b; γ₂⟩

        γ ⊢ e ⇓ ⟨v; γ₁⟩
        ---------------------- γ₂ = γ₁[x=v]
        γ ⊢ EAss x e ⇓ ⟨v; γ₂⟩



Question 5 (Compilation)
========================

1. Statement by statement, translate the example program of Question 1 to Jasmin.
(Do not optimize the program before translation!)
It is not necessary to remember exactly the names of the JVM instructions—only
what arguments they take and how they work.
Make clear which instructions come from which statement, and determine the stack
and local variable limits. (9p)
```

        bool f (int x, bool b) {
          int y = 127;
          if (b || (y = 42) <= x) {
            bool x = (y <= 100);
            return (x || b);
          } else return (x <= 55);
        }


        .method f(IZ)Z
          .limit locals 4
          .limit stack 2

          ;; int y = 127;

          bipush 127
          istore 2

          ;; if (b || (y = 42) <= x)

          ;; b
          iload 1
          ifne L_true

          ;; (y = 42)
          bipush 42
          istore 2
          iload 2

          ;; ... <= x
          iload 0
          if_icmple L_true

          ;; else return (x <= 55);
          iload 0
          bipush 55
          if_icmple L_return_true

          bipush 0
          ireturn

        L_return_true:
          bipush 1
          ireturn

        L_true:
          ;;   bool x = (y <= 100);
          iload 2
          bipush 100
          if_icmple L_assign_true
          bipush 0
          istore 3
          goto L_return_or

        L_assign_true:
          bipush 1
          istore 3

          ;;   return (x || b);
        L_return_or:
          iload 3
          ifne L_return_true
          iload 1
          ireturn

        .end method
```

2. Give the small-step semantics of the JVM instructions you used in the Jasmin code
in part 1 (except for return instructions). Write the semantics in the form

         i : (P,V,S) −→(P′,V′,S′)

where (P,V,S) is the program counter, variable store, and stack before execution
of instruction i, and (P′,V′,S′) are the respective values after the execution. For
adjusting the program counter, you can assume that each instruction has size 1.
(7p)

      bipush v     : (P, V, S)     → (P+1, V, S.v)
      istore a     : (P, V, S.v)   → (P+1, V[a=v], S)
      iload  a     : (P, V, S)     → (P+1, V, S.V[a])
      ifne L       : (P, V, S.v)   → (L, V, S)          if v ≠ 0
      ifne L       : (P, V, S.v)   → (P+1, V, S)        if v = 0
      if_icmple L  : (P, V, S.v.w) → (L, V, S)          if v ≤ w
      if_icmple L  : (P, V, S.v.w) → (P+1, V, S)        otherwise
      goto L       : (P, V, S)     → (L, V, S)

Question 6 (Functional languages)
=================================

1. The following grammar describes a tiny simply-typed sub-language of Haskell.

```
   x                                 identifier
   i ::= 0 |1 |−1 |2 |−2 |...        integer literal
   e ::= i | e + e | x | λx→e | e e  expression
   t ::= Int | t → t                 type
```
Application e1 e2 is left-associative, the arrow t1 →t2 is right-associative. Application
binds strongest, then addition, then λ-abstraction.
For the following typing judgements Γ ⊢e : t, decide whether they are valid or not.
Your answer can be just “valid” or “not valid”, but you may also provide a justification
why some judgement is valid or invalid.

        (a) f : (Int → Int) → (Int → Int) ⊢ (λx → f x) (λf → f) : Int → Int
        yes

        (b) h : (Int → Int) → Int ⊢ λy → y (h y) : (Int → Int) → Int
        yes

        (c) h : (Int → Int) → Int ⊢ λx → h (h x) : (Int →Int) →Int
        no

        (d) z : Int, y : Int → Int ⊢ (λf → y + 1) z : Int
        no

        (e) f : Int → Int ⊢ λ g → f (f 1 + f g) : Int → Int
        yes


2. For each of the following terms, decide whether it evaluates more efficiently (in the
sense of fewer reductions) in call-by-name or call-by-value. Your answer can be just “call-
by-name” or “call-by-value”, but you can also add a justification why you think so. Same
rules for multiple choice as in part 1. (5p)

        (a) (λx → λy → x + x) (1 + 2 + 3 + 4) (5 + 6 + 7)

        ✓ cbv: 6 adds (3 + 2 + 1)
        x cbn: 7 adds (3 + 3 + 1)

        (b) (λx → λy → y + y) (λu → (λz → z z) (λz → z z)) (1 + 2)
        ✓ cbv: 2 adds
        x cnv: 3 adds

        (c) (λx → x + x) ((λy → λz → z + z) (1 + 2 + 3) (4 + 5))
        ✓ cbv: 5 adds
        x cbn: 7 adds

        (d) (λx → λy → y + y) ((λz → z z) (λz → z z)) (1 + 2 + 3)
        x cbv: ∞
        ✓ cbn: 5 adds

        (e) (λx → λy → x + x) (1 + 2) (3 + 4 + 5)
        x cbv:  4 adds
        ✓ cbn : 3 adds
