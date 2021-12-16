PLT Exam of Jan 2020, solution transcript of 2021-12-16

# Q3

    Seq.   S ::= P M ;

    Plus.  P ::= P A "+";
    None.  P ::= ;

    Minus. M ::= A "-" M ;
    Done.  M ::= A       ;

    X.     A ::= "x" ;
    Y.     A ::= "y" ;


      . x + y - x    -- reduce None
    P . x + y - x    -- shift x
    P x . + y - x    -- reduce X
    P A . + y - x    -- shift +
    P A + . y - x    -- reduce Plus
    P     . y - x    -- shift y
    P y     . - x    -- reduce Y
    P A     . - x    -- shift -
    P A -     . x    -- shift x
    P A - x     .    -- reduce X
    P A - A     .    -- reduce Done
    P A - M     .    -- reduce Minus
    P M         .    -- reduce Seq
    S                -- accept


# Q4

## Type checking

Judgement Γ ⊢ s ⇒ Γ'
Contexts Γ are .-separated lists of finite maps Δ from identifiers x to types t.

- Blocks  Γ ⊢ b ⇒ Γ'

        Γ.ε ⊢ ss ⇒ Γ'
        ------------------------
        Γ   ⊢ { ss } ⇒ Γ

- Statement sequences  Γ ⊢ ss ⇒ Γ'

        ---------
        Γ ⊢ ε ⇒ Γ

        Γ ⊢ s ⇒ Γ₁    Γ₁ ⊢ ss ⇒ Γ₂
        ---------------------------
        Γ ⊢ s ss ⇒ Γ₂

- Statements Γ ⊢ s ⇒ Γ'

  Initialization "t x; x = e;"

        Γ.(Δ,x:t) ⊢ e : t
        ----------------------------- x ∉ Δ
        Γ.Δ ⊢ (t x = e;) ⇒ Γ.(Δ,x:t)

  Expression as statement

        Γ ⊢ e : t
        -----------
        Γ ⊢ e; ⇒ Γ

  If-else

        Γ ⊢ e : bool     Γ.ε ⊢ s₁ ⇒ _    Γ.ε ⊢ s₂ ⇒ _
        ----------------------------------------------
        Γ ⊢ if (e) s₁ else s₂ ⇒ Γ


### Alternative for type checker: only return addition to context

Judgement Γ ⊢ s ⇒ Δ
Contexts Γ are .-separated lists of finite maps Δ from identifiers x to types t.

- Blocks  Γ ⊢ b

        Γ.ε ⊢ ss
        ------------
        Γ   ⊢ { ss }

- Statement sequences  Γ ⊢ ss

        -----
        Γ ⊢ ε

        Γ.Δ ⊢ s ⇒ Δ'    Γ.(Δ,Δ') ⊢ ss
        -------------------------------
        Γ.Δ ⊢ s ss

- Statements Γ ⊢ s ⇒ Δ

  Initialization "t x; x = e;"

        Γ.(Δ,x:t) ⊢ e : t
        ----------------------------- x ∉ Δ
        Γ.Δ ⊢ (t x = e;) ⇒ (x:t)

  Expression as statement

        Γ ⊢ e : t
        -----------
        Γ ⊢ e; ⇒ ()

  If-else

        Γ ⊢ e : bool     Γ.ε ⊢ s₁ ⇒ _    Γ.ε ⊢ s₂ ⇒ _
        ----------------------------------------------
        Γ ⊢ if (e) s₁ else s₂ ⇒ ()


## Interpretation of expressions

Judgement: γ ⊢ e ⇒ ⟨v; γ'⟩

An environment γ is a finite map from identifiers x to values v.
A value is an integer or boolean literal.

Expressions:

– boolean literal true or false

      γ ⊢ true  ⇒ ⟨ true;  γ ⟩
      γ ⊢ false ⇒ ⟨ false; γ ⟩

– integer literal

      γ ⊢ i ⇒ ⟨ i; γ ⟩

– function call with a list of comma-separated arguments
  Function:

      t f (t₁ x₁, ... tₙ xₙ) b

      γ    ⊢ e₁ ⇒ ⟨ v₁; γ₁ ⟩
      γ₁   ⊢ e₂ ⇒ ⟨ v₂; γ₂ ⟩
      ...
      γₙ-₁ ⊢ eₙ ⇒ ⟨ vₙ; γₙ ⟩
      x₁=v₁,...,xₙ=vₙ ⊢ b ⇒ v
      ----------------------------
      γ ⊢ f(e₁,...,eₙ) ⇒ ⟨ v; γₙ ⟩

– post-increment of an identifier, e.g., x++

      -------------------------- γ(x)=i
      γ ⊢ x++ ⇒ ⟨ i; γ[x=i+1] ⟩

– addition (+), left associative

      γ  ⊢ e₁ ⇒ ⟨ i₁; γ₁ ⟩
      γ₁ ⊢ e₂ ⇒ ⟨ i₂; γ₂ ⟩
      -----------------------------
      γ ⊢ e₁ + e₂ ⇒ ⟨ i₁ + i₂; γ₂ ⟩

# Q5  Compilation

## Compilation schemes

• Block: a sequence of statements enclosed between { and }

    compile( { ss } ):
      newBlock
      for (s : ss) compile (s)
      popBlock

• Statement:

– block
– initializing variable declaration, e.g., int x = e;

    compile ( t x = e; ):
      a <- addVar t x
      compile (e)
      emit (istore a)

– statement formed from an expression by adding a semicolon ;

    compile ( e; ):
      compile (e)
      emit (pop)

– if statement with else

    compile ( if e s₁ else s₂ ):
      Lfalse,Lend <- newLabel
      compile(e)
      emit (ifeq Lfalse)
      compile (s₁)
      emit (goto Lend)
      emit (Lfalse:)
      compile (s₂)
      emit (Lend:)

• Expression:

– boolean literal true or false

    compile (true):
      emit (ldc 1)
    compile (false):
      emit (ldc 0)

– integer literal

    compile (i):
      emit (ldc i)

– function call with a list of comma-separated arguments

    compile ( f(es) )
      fname ← lookupFun f
      for (e : es) compile(e)
      emit (invokestatic fname)

– post-increment of an identifier, e.g., x++

    compile ( x++ ):
      a ← lookupVar x
      emit (iload a)
      emit (dup)
      emit (ldc 1)
      emit (iadd)
      emit (istore a)

– addition (+), left associative

    compile ( e₁ + e₂ ):
      compile (e₁)
      compile (e₂)
      emit (iadd)


## Small-step semantics

    c : (P,V,S) → (P',V',S')

    istore a: (P, V, S.i)   → (P+1, V[a=i], S)
    iload a : (P, V, S)     → (P+1, V, S.V[a])
    pop     : (P, V, S.i)   → (P+1, V, S)
    dup     : (P, V, S.i)   → (P+1, V, S.i.i)
    ldc i   : (P, V, S)     → (P+1, V, S.i)
    iadd    : (P, V, S.i.j) → (P+1, V, S.(i+j))
    goto l  : (P, V, S)     → (l, V, S)
    ifeq l  : (P, V, S.0)   → (l, V, S)
    ifeq l  : (P, V, S.i)   → (P+1, V, S)   if i ≠ 0
    invokestatic f
            : (P, V, S.v₁...vₙ) → (P+1, V, S.v)
      if f(v₁,...,vₙ)=v


# Q6  Call-by-name interpreter

Judgement: γ ⊢ e ⇒ v

Environments γ,δ are finite maps from identifiers x to closures ⟨ e; δ ⟩.
Values v are integer literals i or function closures ⟨ λx→e; δ ⟩.

     δ ⊢ e ⇒ v
     ---------- γ(x) = ⟨ e; δ ⟩
     γ ⊢ x ⇒ v

     ----------------------
     γ ⊢ λx→e ⇒ ⟨ λx→e; γ ⟩

     ---------------
     γ ⊢ i ⇒ i

     γ ⊢ e₁ ⇒ i₁
     γ ⊢ e₂ ⇒ i₂
     ---------------------
     γ ⊢ e₁ + e₂ ⇒ i₁ + i₂


     γ        ⊢ f ⇒ ⟨ λx→e'; δ ⟩
     δ,x=⟨e;γ⟩ ⊢ e' ⇒ v
     ------------------------
     γ        ⊢ f e ⇒ v
