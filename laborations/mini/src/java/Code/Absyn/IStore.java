package Code.Absyn; // Java Package generated by the BNF Converter.

public class IStore  extends Ins {
  public final Integer integer_;
  public IStore(Integer p1) { integer_ = p1; }

  public <R,A> R accept(Code.Absyn.Ins.Visitor<R,A> v, A arg) { return v.visit(this, arg); }

  public boolean equals(Object o) {
    if (this == o) return true;
    if (o instanceof Code.Absyn.IStore) {
      Code.Absyn.IStore x = (Code.Absyn.IStore)o;
      return this.integer_.equals(x.integer_);
    }
    return false;
  }

  public int hashCode() {
    return this.integer_.hashCode();
  }


}
