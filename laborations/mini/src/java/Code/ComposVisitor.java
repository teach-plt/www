package Code;
import Code.Absyn.*;
/** BNFC-Generated Composition Visitor
*/

public class ComposVisitor<A> implements
  Code.Absyn.Ins.Visitor<Code.Absyn.Ins,A>
{
/* Ins */
    public Ins visit(Code.Absyn.ILoad p, A arg)
    {
      Integer integer_ = p.integer_;
      return new Code.Absyn.ILoad(integer_);
    }    public Ins visit(Code.Absyn.IStore p, A arg)
    {
      Integer integer_ = p.integer_;
      return new Code.Absyn.IStore(integer_);
    }    public Ins visit(Code.Absyn.IAdd p, A arg)
    {
      return new Code.Absyn.IAdd();
    }    public Ins visit(Code.Absyn.ISub p, A arg)
    {
      return new Code.Absyn.ISub();
    }    public Ins visit(Code.Absyn.IMul p, A arg)
    {
      return new Code.Absyn.IMul();
    }    public Ins visit(Code.Absyn.IDiv p, A arg)
    {
      return new Code.Absyn.IDiv();
    }    public Ins visit(Code.Absyn.ILit p, A arg)
    {
      Integer integer_ = p.integer_;
      return new Code.Absyn.ILit(integer_);
    }    public Ins visit(Code.Absyn.DLoad p, A arg)
    {
      Integer integer_ = p.integer_;
      return new Code.Absyn.DLoad(integer_);
    }    public Ins visit(Code.Absyn.DStore p, A arg)
    {
      Integer integer_ = p.integer_;
      return new Code.Absyn.DStore(integer_);
    }    public Ins visit(Code.Absyn.DAdd p, A arg)
    {
      return new Code.Absyn.DAdd();
    }    public Ins visit(Code.Absyn.DSub p, A arg)
    {
      return new Code.Absyn.DSub();
    }    public Ins visit(Code.Absyn.DMul p, A arg)
    {
      return new Code.Absyn.DMul();
    }    public Ins visit(Code.Absyn.DDiv p, A arg)
    {
      return new Code.Absyn.DDiv();
    }    public Ins visit(Code.Absyn.DLit p, A arg)
    {
      Double double_ = p.double_;
      return new Code.Absyn.DLit(double_);
    }    public Ins visit(Code.Absyn.I2D p, A arg)
    {
      return new Code.Absyn.I2D();
    }    public Ins visit(Code.Absyn.IPrint p, A arg)
    {
      return new Code.Absyn.IPrint();
    }    public Ins visit(Code.Absyn.DPrint p, A arg)
    {
      return new Code.Absyn.DPrint();
    }
}
