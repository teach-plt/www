package Code.Absyn; // Java Package generated by the BNF Converter.

public class DAdd  extends Ins {
  public DAdd() { }

  public <R,A> R accept(Code.Absyn.Ins.Visitor<R,A> v, A arg) { return v.visit(this, arg); }

  public boolean equals(Object o) {
    if (this == o) return true;
    if (o instanceof Code.Absyn.DAdd) {
      return true;
    }
    return false;
  }

  public int hashCode() {
    return 37;
  }


}
