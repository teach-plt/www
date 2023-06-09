package MiniJS.Absyn; // Java Package generated by the BNF Converter.

public abstract class Exp implements java.io.Serializable {
  public abstract <R,A> R accept(Exp.Visitor<R,A> v, A arg);
  public interface Visitor <R,A> {
    public R visit(MiniJS.Absyn.EVar p, A arg);
    public R visit(MiniJS.Absyn.EInt p, A arg);
    public R visit(MiniJS.Absyn.EDouble p, A arg);
    public R visit(MiniJS.Absyn.ETimes p, A arg);
    public R visit(MiniJS.Absyn.EDiv p, A arg);
    public R visit(MiniJS.Absyn.EPlus p, A arg);
    public R visit(MiniJS.Absyn.EMinus p, A arg);

  }

}
