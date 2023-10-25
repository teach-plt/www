package Ann;

import Ann.Absyn.*;
import java.util.Collections;
import java.util.List;
import java.util.ArrayList;

/** BNFC-Generated Fold Visitor */
public abstract class FoldVisitor<R,A> implements AllVisitor<R,A> {
    public abstract R leaf(A arg);
    public abstract R combine(R x, R y, A arg);

/* Program */
    public R visit(Ann.Absyn.Prg p, A arg) {
      R r = leaf(arg);
      for (Stm x : p.liststm_)
      {
        r = combine(x.accept(this, arg), r, arg);
      }
      return r;
    }

/* Stm */
    public R visit(Ann.Absyn.SAssign p, A arg) {
      R r = leaf(arg);
      r = combine(p.type_.accept(this, arg), r, arg);
      r = combine(p.exp_.accept(this, arg), r, arg);
      return r;
    }
    public R visit(Ann.Absyn.SPrint p, A arg) {
      R r = leaf(arg);
      r = combine(p.type_.accept(this, arg), r, arg);
      r = combine(p.exp_.accept(this, arg), r, arg);
      return r;
    }

/* Exp */
    public R visit(Ann.Absyn.EInt p, A arg) {
      R r = leaf(arg);
      return r;
    }
    public R visit(Ann.Absyn.EDouble p, A arg) {
      R r = leaf(arg);
      return r;
    }
    public R visit(Ann.Absyn.EVar p, A arg) {
      R r = leaf(arg);
      r = combine(p.type_.accept(this, arg), r, arg);
      return r;
    }
    public R visit(Ann.Absyn.EArith p, A arg) {
      R r = leaf(arg);
      r = combine(p.type_.accept(this, arg), r, arg);
      r = combine(p.exp_1.accept(this, arg), r, arg);
      r = combine(p.op_.accept(this, arg), r, arg);
      r = combine(p.exp_2.accept(this, arg), r, arg);
      return r;
    }
    public R visit(Ann.Absyn.EI2D p, A arg) {
      R r = leaf(arg);
      r = combine(p.exp_.accept(this, arg), r, arg);
      return r;
    }

/* Type */
    public R visit(Ann.Absyn.TInt p, A arg) {
      R r = leaf(arg);
      return r;
    }
    public R visit(Ann.Absyn.TDouble p, A arg) {
      R r = leaf(arg);
      return r;
    }

/* Op */
    public R visit(Ann.Absyn.Times p, A arg) {
      R r = leaf(arg);
      return r;
    }
    public R visit(Ann.Absyn.Div p, A arg) {
      R r = leaf(arg);
      return r;
    }
    public R visit(Ann.Absyn.Plus p, A arg) {
      R r = leaf(arg);
      return r;
    }
    public R visit(Ann.Absyn.Minus p, A arg) {
      R r = leaf(arg);
      return r;
    }


}
