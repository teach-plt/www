package MiniJS;
import MiniJS.Absyn.*;
/** BNFC-Generated Composition Visitor
*/

public class ComposVisitor<A> implements
  MiniJS.Absyn.Program.Visitor<MiniJS.Absyn.Program,A>,
  MiniJS.Absyn.Stm.Visitor<MiniJS.Absyn.Stm,A>,
  MiniJS.Absyn.Exp.Visitor<MiniJS.Absyn.Exp,A>
{
/* Program */
    public Program visit(MiniJS.Absyn.Prg p, A arg)
    {
      ListStm liststm_ = new ListStm();
      for (Stm x : p.liststm_)
      {
        liststm_.add(x.accept(this,arg));
      }
      return new MiniJS.Absyn.Prg(liststm_);
    }
/* Stm */
    public Stm visit(MiniJS.Absyn.SAssign p, A arg)
    {
      String ident_ = p.ident_;
      Exp exp_ = p.exp_.accept(this, arg);
      return new MiniJS.Absyn.SAssign(ident_, exp_);
    }    public Stm visit(MiniJS.Absyn.SPrint p, A arg)
    {
      Exp exp_ = p.exp_.accept(this, arg);
      return new MiniJS.Absyn.SPrint(exp_);
    }
/* Exp */
    public Exp visit(MiniJS.Absyn.EVar p, A arg)
    {
      String ident_ = p.ident_;
      return new MiniJS.Absyn.EVar(ident_);
    }    public Exp visit(MiniJS.Absyn.EInt p, A arg)
    {
      Integer integer_ = p.integer_;
      return new MiniJS.Absyn.EInt(integer_);
    }    public Exp visit(MiniJS.Absyn.EDouble p, A arg)
    {
      Double double_ = p.double_;
      return new MiniJS.Absyn.EDouble(double_);
    }    public Exp visit(MiniJS.Absyn.ETimes p, A arg)
    {
      Exp exp_1 = p.exp_1.accept(this, arg);
      Exp exp_2 = p.exp_2.accept(this, arg);
      return new MiniJS.Absyn.ETimes(exp_1, exp_2);
    }    public Exp visit(MiniJS.Absyn.EDiv p, A arg)
    {
      Exp exp_1 = p.exp_1.accept(this, arg);
      Exp exp_2 = p.exp_2.accept(this, arg);
      return new MiniJS.Absyn.EDiv(exp_1, exp_2);
    }    public Exp visit(MiniJS.Absyn.EPlus p, A arg)
    {
      Exp exp_1 = p.exp_1.accept(this, arg);
      Exp exp_2 = p.exp_2.accept(this, arg);
      return new MiniJS.Absyn.EPlus(exp_1, exp_2);
    }    public Exp visit(MiniJS.Absyn.EMinus p, A arg)
    {
      Exp exp_1 = p.exp_1.accept(this, arg);
      Exp exp_2 = p.exp_2.accept(this, arg);
      return new MiniJS.Absyn.EMinus(exp_1, exp_2);
    }
}
