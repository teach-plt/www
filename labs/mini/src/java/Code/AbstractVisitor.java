package Code;
import Code.Absyn.*;
/** BNFC-Generated Abstract Visitor */
public class AbstractVisitor<R,A> implements AllVisitor<R,A> {
/* Ins */
    public R visit(Code.Absyn.ILoad p, A arg) { return visitDefault(p, arg); }
    public R visit(Code.Absyn.IStore p, A arg) { return visitDefault(p, arg); }
    public R visit(Code.Absyn.IAdd p, A arg) { return visitDefault(p, arg); }
    public R visit(Code.Absyn.ISub p, A arg) { return visitDefault(p, arg); }
    public R visit(Code.Absyn.IMul p, A arg) { return visitDefault(p, arg); }
    public R visit(Code.Absyn.IDiv p, A arg) { return visitDefault(p, arg); }
    public R visit(Code.Absyn.ILit p, A arg) { return visitDefault(p, arg); }
    public R visit(Code.Absyn.DLoad p, A arg) { return visitDefault(p, arg); }
    public R visit(Code.Absyn.DStore p, A arg) { return visitDefault(p, arg); }
    public R visit(Code.Absyn.DAdd p, A arg) { return visitDefault(p, arg); }
    public R visit(Code.Absyn.DSub p, A arg) { return visitDefault(p, arg); }
    public R visit(Code.Absyn.DMul p, A arg) { return visitDefault(p, arg); }
    public R visit(Code.Absyn.DDiv p, A arg) { return visitDefault(p, arg); }
    public R visit(Code.Absyn.DLit p, A arg) { return visitDefault(p, arg); }
    public R visit(Code.Absyn.I2D p, A arg) { return visitDefault(p, arg); }
    public R visit(Code.Absyn.IPrint p, A arg) { return visitDefault(p, arg); }
    public R visit(Code.Absyn.DPrint p, A arg) { return visitDefault(p, arg); }
    public R visitDefault(Code.Absyn.Ins p, A arg) {
      throw new IllegalArgumentException(this.getClass().getName() + ": " + p);
    }

}
