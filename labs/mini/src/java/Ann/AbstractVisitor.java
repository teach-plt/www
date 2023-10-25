package Ann;
import Ann.Absyn.*;
/** BNFC-Generated Abstract Visitor */
public class AbstractVisitor<R,A> implements AllVisitor<R,A> {
/* Program */
    public R visit(Ann.Absyn.Prg p, A arg) { return visitDefault(p, arg); }
    public R visitDefault(Ann.Absyn.Program p, A arg) {
      throw new IllegalArgumentException(this.getClass().getName() + ": " + p);
    }
/* Stm */
    public R visit(Ann.Absyn.SAssign p, A arg) { return visitDefault(p, arg); }
    public R visit(Ann.Absyn.SPrint p, A arg) { return visitDefault(p, arg); }
    public R visitDefault(Ann.Absyn.Stm p, A arg) {
      throw new IllegalArgumentException(this.getClass().getName() + ": " + p);
    }
/* Exp */
    public R visit(Ann.Absyn.EInt p, A arg) { return visitDefault(p, arg); }
    public R visit(Ann.Absyn.EDouble p, A arg) { return visitDefault(p, arg); }
    public R visit(Ann.Absyn.EVar p, A arg) { return visitDefault(p, arg); }
    public R visit(Ann.Absyn.EArith p, A arg) { return visitDefault(p, arg); }
    public R visit(Ann.Absyn.EI2D p, A arg) { return visitDefault(p, arg); }
    public R visitDefault(Ann.Absyn.Exp p, A arg) {
      throw new IllegalArgumentException(this.getClass().getName() + ": " + p);
    }
/* Type */
    public R visit(Ann.Absyn.TInt p, A arg) { return visitDefault(p, arg); }
    public R visit(Ann.Absyn.TDouble p, A arg) { return visitDefault(p, arg); }
    public R visitDefault(Ann.Absyn.Type p, A arg) {
      throw new IllegalArgumentException(this.getClass().getName() + ": " + p);
    }
/* Op */
    public R visit(Ann.Absyn.Times p, A arg) { return visitDefault(p, arg); }
    public R visit(Ann.Absyn.Div p, A arg) { return visitDefault(p, arg); }
    public R visit(Ann.Absyn.Plus p, A arg) { return visitDefault(p, arg); }
    public R visit(Ann.Absyn.Minus p, A arg) { return visitDefault(p, arg); }
    public R visitDefault(Ann.Absyn.Op p, A arg) {
      throw new IllegalArgumentException(this.getClass().getName() + ": " + p);
    }

}
