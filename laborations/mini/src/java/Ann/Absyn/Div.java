package Ann.Absyn; // Java Package generated by the BNF Converter.

public class Div  extends Op {
  public Div() { }

  public <R,A> R accept(Ann.Absyn.Op.Visitor<R,A> v, A arg) { return v.visit(this, arg); }

  public boolean equals(Object o) {
    if (this == o) return true;
    if (o instanceof Ann.Absyn.Div) {
      return true;
    }
    return false;
  }

  public int hashCode() {
    return 37;
  }


}
