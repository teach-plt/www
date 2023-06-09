package MiniJS.Absyn; // Java Package generated by the BNF Converter.

public class EVar  extends Exp {
  public final String ident_;
  public EVar(String p1) { ident_ = p1; }

  public <R,A> R accept(MiniJS.Absyn.Exp.Visitor<R,A> v, A arg) { return v.visit(this, arg); }

  public boolean equals(Object o) {
    if (this == o) return true;
    if (o instanceof MiniJS.Absyn.EVar) {
      MiniJS.Absyn.EVar x = (MiniJS.Absyn.EVar)o;
      return this.ident_.equals(x.ident_);
    }
    return false;
  }

  public int hashCode() {
    return this.ident_.hashCode();
  }


}
