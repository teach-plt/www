package Code;

import Code.Absyn.*;
import java.util.Collections;
import java.util.List;
import java.util.ArrayList;

/** BNFC-Generated Fold Visitor */
public abstract class FoldVisitor<R,A> implements AllVisitor<R,A> {
    public abstract R leaf(A arg);
    public abstract R combine(R x, R y, A arg);

/* Ins */
    public R visit(Code.Absyn.ILoad p, A arg) {
      R r = leaf(arg);
      return r;
    }
    public R visit(Code.Absyn.IStore p, A arg) {
      R r = leaf(arg);
      return r;
    }
    public R visit(Code.Absyn.IAdd p, A arg) {
      R r = leaf(arg);
      return r;
    }
    public R visit(Code.Absyn.ISub p, A arg) {
      R r = leaf(arg);
      return r;
    }
    public R visit(Code.Absyn.IMul p, A arg) {
      R r = leaf(arg);
      return r;
    }
    public R visit(Code.Absyn.IDiv p, A arg) {
      R r = leaf(arg);
      return r;
    }
    public R visit(Code.Absyn.ILit p, A arg) {
      R r = leaf(arg);
      return r;
    }
    public R visit(Code.Absyn.DLoad p, A arg) {
      R r = leaf(arg);
      return r;
    }
    public R visit(Code.Absyn.DStore p, A arg) {
      R r = leaf(arg);
      return r;
    }
    public R visit(Code.Absyn.DAdd p, A arg) {
      R r = leaf(arg);
      return r;
    }
    public R visit(Code.Absyn.DSub p, A arg) {
      R r = leaf(arg);
      return r;
    }
    public R visit(Code.Absyn.DMul p, A arg) {
      R r = leaf(arg);
      return r;
    }
    public R visit(Code.Absyn.DDiv p, A arg) {
      R r = leaf(arg);
      return r;
    }
    public R visit(Code.Absyn.DLit p, A arg) {
      R r = leaf(arg);
      return r;
    }
    public R visit(Code.Absyn.I2D p, A arg) {
      R r = leaf(arg);
      return r;
    }
    public R visit(Code.Absyn.IPrint p, A arg) {
      R r = leaf(arg);
      return r;
    }
    public R visit(Code.Absyn.DPrint p, A arg) {
      R r = leaf(arg);
      return r;
    }


}
