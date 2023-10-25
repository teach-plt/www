package Ann;
import Ann.Absyn.*;
/** BNFC-Generated Composition Visitor
*/

public class ComposVisitor<A> implements
  Ann.Absyn.Program.Visitor<Ann.Absyn.Program,A>,
  Ann.Absyn.Stm.Visitor<Ann.Absyn.Stm,A>,
  Ann.Absyn.Exp.Visitor<Ann.Absyn.Exp,A>,
  Ann.Absyn.Type.Visitor<Ann.Absyn.Type,A>,
  Ann.Absyn.Op.Visitor<Ann.Absyn.Op,A>
{
/* Program */
    public Program visit(Ann.Absyn.Prg p, A arg)
    {
      ListStm liststm_ = new ListStm();
      for (Stm x : p.liststm_)
      {
        liststm_.add(x.accept(this,arg));
      }
      return new Ann.Absyn.Prg(liststm_);
    }
/* Stm */
    public Stm visit(Ann.Absyn.SAssign p, A arg)
    {
      String ident_ = p.ident_;
      Type type_ = p.type_.accept(this, arg);
      Exp exp_ = p.exp_.accept(this, arg);
      return new Ann.Absyn.SAssign(ident_, type_, exp_);
    }    public Stm visit(Ann.Absyn.SPrint p, A arg)
    {
      Type type_ = p.type_.accept(this, arg);
      Exp exp_ = p.exp_.accept(this, arg);
      return new Ann.Absyn.SPrint(type_, exp_);
    }
/* Exp */
    public Exp visit(Ann.Absyn.EInt p, A arg)
    {
      Integer integer_ = p.integer_;
      return new Ann.Absyn.EInt(integer_);
    }    public Exp visit(Ann.Absyn.EDouble p, A arg)
    {
      Double double_ = p.double_;
      return new Ann.Absyn.EDouble(double_);
    }    public Exp visit(Ann.Absyn.EVar p, A arg)
    {
      Type type_ = p.type_.accept(this, arg);
      String ident_ = p.ident_;
      return new Ann.Absyn.EVar(type_, ident_);
    }    public Exp visit(Ann.Absyn.EArith p, A arg)
    {
      Type type_ = p.type_.accept(this, arg);
      Exp exp_1 = p.exp_1.accept(this, arg);
      Op op_ = p.op_.accept(this, arg);
      Exp exp_2 = p.exp_2.accept(this, arg);
      return new Ann.Absyn.EArith(type_, exp_1, op_, exp_2);
    }    public Exp visit(Ann.Absyn.EI2D p, A arg)
    {
      Exp exp_ = p.exp_.accept(this, arg);
      return new Ann.Absyn.EI2D(exp_);
    }
/* Type */
    public Type visit(Ann.Absyn.TInt p, A arg)
    {
      return new Ann.Absyn.TInt();
    }    public Type visit(Ann.Absyn.TDouble p, A arg)
    {
      return new Ann.Absyn.TDouble();
    }
/* Op */
    public Op visit(Ann.Absyn.Times p, A arg)
    {
      return new Ann.Absyn.Times();
    }    public Op visit(Ann.Absyn.Div p, A arg)
    {
      return new Ann.Absyn.Div();
    }    public Op visit(Ann.Absyn.Plus p, A arg)
    {
      return new Ann.Absyn.Plus();
    }    public Op visit(Ann.Absyn.Minus p, A arg)
    {
      return new Ann.Absyn.Minus();
    }
}
