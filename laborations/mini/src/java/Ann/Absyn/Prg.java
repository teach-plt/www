package Ann.Absyn; // Java Package generated by the BNF Converter.

public class Prg  extends Program {
  public final ListStm liststm_;
  public Prg(ListStm p1) { liststm_ = p1; }

  public <R,A> R accept(Ann.Absyn.Program.Visitor<R,A> v, A arg) { return v.visit(this, arg); }

  public boolean equals(Object o) {
    if (this == o) return true;
    if (o instanceof Ann.Absyn.Prg) {
      Ann.Absyn.Prg x = (Ann.Absyn.Prg)o;
      return this.liststm_.equals(x.liststm_);
    }
    return false;
  }

  public int hashCode() {
    return this.liststm_.hashCode();
  }


}
