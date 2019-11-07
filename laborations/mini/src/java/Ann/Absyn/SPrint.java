package Ann.Absyn; // Java Package generated by the BNF Converter.

public class SPrint  extends Stm {
  public final Type type_;
  public final Exp exp_;
  public SPrint(Type p1, Exp p2) { type_ = p1; exp_ = p2; }

  public <R,A> R accept(Ann.Absyn.Stm.Visitor<R,A> v, A arg) { return v.visit(this, arg); }

  public boolean equals(Object o) {
    if (this == o) return true;
    if (o instanceof Ann.Absyn.SPrint) {
      Ann.Absyn.SPrint x = (Ann.Absyn.SPrint)o;
      return this.type_.equals(x.type_) && this.exp_.equals(x.exp_);
    }
    return false;
  }

  public int hashCode() {
    return 37*(this.type_.hashCode())+this.exp_.hashCode();
  }


}
