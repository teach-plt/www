package MiniJS;
import MiniJS.Absyn.*;
/** BNFC-Generated Abstract Visitor */
public class AbstractVisitor<R,A> implements AllVisitor<R,A> {
/* Program */
    public R visit(MiniJS.Absyn.Prg p, A arg) { return visitDefault(p, arg); }
    public R visitDefault(MiniJS.Absyn.Program p, A arg) {
      throw new IllegalArgumentException(this.getClass().getName() + ": " + p);
    }
/* Stm */
    public R visit(MiniJS.Absyn.SAssign p, A arg) { return visitDefault(p, arg); }
    public R visit(MiniJS.Absyn.SPrint p, A arg) { return visitDefault(p, arg); }
    public R visitDefault(MiniJS.Absyn.Stm p, A arg) {
      throw new IllegalArgumentException(this.getClass().getName() + ": " + p);
    }
/* Exp */
    public R visit(MiniJS.Absyn.EVar p, A arg) { return visitDefault(p, arg); }
    public R visit(MiniJS.Absyn.EInt p, A arg) { return visitDefault(p, arg); }
    public R visit(MiniJS.Absyn.EDouble p, A arg) { return visitDefault(p, arg); }

    public R visit(MiniJS.Absyn.ETimes p, A arg) { return visitDefault(p, arg); }
    public R visit(MiniJS.Absyn.EDiv p, A arg) { return visitDefault(p, arg); }

    public R visit(MiniJS.Absyn.EPlus p, A arg) { return visitDefault(p, arg); }
    public R visit(MiniJS.Absyn.EMinus p, A arg) { return visitDefault(p, arg); }

    public R visitDefault(MiniJS.Absyn.Exp p, A arg) {
      throw new IllegalArgumentException(this.getClass().getName() + ": " + p);
    }

}
