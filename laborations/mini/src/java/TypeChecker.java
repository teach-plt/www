// TypeChecker for MiniJS, returning type-annotated syntax.
// Adapted from  MiniJS.ComposVisitor.

import Ann.Absyn.*;
import java.util.*;

public class TypeChecker implements
  MiniJS.Absyn.Program.Visitor<Program,Void>,
  MiniJS.Absyn.Stm.Visitor<Stm,Void>,
  MiniJS.Absyn.Exp.Visitor<Exp,Void>
{
    // Share types
    public static final Type INT    = new TInt();
    public static final Type DOUBLE = new TDouble();

    // Share operators
    public static final Op TIMES  = new Times();
    public static final Op DIV    = new Div();
    public static final Op PLUS   = new Plus();
    public static final Op MINUS  = new Minus();

    // Typing context
    private Map<String,Type> cxt;

    // Type of the expression we checked last
    // (saves us from returning a pair).
    private Type t;

    public static Program typeCheck (MiniJS.Absyn.Program prg) {
      return prg.accept (new TypeChecker(), null);
    }

    // Program

    public Program visit(MiniJS.Absyn.Prg p, Void arg)
    {
      cxt = new TreeMap();
      ListStm liststm_ = new ListStm();
      for (MiniJS.Absyn.Stm s : p.liststm_)
      {
        liststm_.add(s.accept(this,arg));
      }
      return new Prg(liststm_);
    }

    // Stm

    public Stm visit(MiniJS.Absyn.SAssign p, Void arg)
    {
      Exp exp_ = p.exp_.accept(this, arg);
      // Get type of variable x.
      Type tx = cxt.get (p.ident_);
      // Add new binding x:t if absent.
      if (tx == null) cxt.put (p.ident_, t);
      // Fail if new type conflicts with old type.
      else if (! tx.equals(t))
        throw new RuntimeException("invalid assignment");
      return new SAssign(p.ident_, t, exp_);
    }
    public Stm visit(MiniJS.Absyn.SPrint p, Void arg)
    {
      Exp exp_ = p.exp_.accept(this, arg);
      return new SPrint(t, exp_);
    }

    // Exp

    // Variable
    public Exp visit(MiniJS.Absyn.EVar p, Void arg)
    {
      t = cxt.get(p.ident_);
      return new EVar(t, p.ident_);
    }

    // Literals
    public Exp visit(MiniJS.Absyn.EInt p, Void arg)
    {
      t = INT;
      return new EInt(p.integer_);
    }
    public Exp visit(MiniJS.Absyn.EDouble p, Void arg)
    {
      t = DOUBLE;
      return new EDouble(p.double_);
    }

    // Arithmetical operations
    public Exp visit(MiniJS.Absyn.ETimes p, Void arg)
    {
      Exp exp_1 = p.exp_1.accept(this, arg);
      Type t1 = t;
      Exp exp_2 = p.exp_2.accept(this, arg);
      Type t2 = t;
      t = maxType (t1, t2);
      return eArith (t, t1, t2, exp_1, TIMES, exp_2);
    }
    public Exp visit(MiniJS.Absyn.EDiv p, Void arg)
    {
      Exp exp_1 = p.exp_1.accept(this, arg);
      Type t1 = t;
      Exp exp_2 = p.exp_2.accept(this, arg);
      Type t2 = t;
      t = maxType (t1, t2);
      return eArith (t, t1, t2, exp_1, DIV, exp_2);
    }
    public Exp visit(MiniJS.Absyn.EPlus p, Void arg)
    {
      Exp exp_1 = p.exp_1.accept(this, arg);
      Type t1 = t;
      Exp exp_2 = p.exp_2.accept(this, arg);
      Type t2 = t;
      t = maxType (t1, t2);
      return eArith (t, t1, t2, exp_1, PLUS, exp_2);
    }
    public Exp visit(MiniJS.Absyn.EMinus p, Void arg)
    {
      Exp exp_1 = p.exp_1.accept(this, arg);
      Type t1 = t;
      Exp exp_2 = p.exp_2.accept(this, arg);
      Type t2 = t;
      t = maxType (t1, t2);
      return eArith (t, t1, t2, exp_1, MINUS, exp_2);
    }

    // Smart constructor for arithmetical expression, inserting casts

    public Exp eArith (Type t, Type t1, Type t2, Exp e1, Op op, Exp e2) {
      return new EArith(t, cast (t1, t, e1), op, cast (t2, t, e2));
    }

    // Subtyping

    public Type maxType (Type t1, Type t2) {
      if (t1 instanceof TInt) return t2;
      else return t1;
    }

    public Exp cast (Type t1, Type t2, Exp e) {
      if (t1 instanceof TInt && t2 instanceof TDouble)
        return new EI2D(e);
      else return e;
    }
}
