package Ann;
import Ann.Absyn.*;
/*** BNFC-Generated Visitor Design Pattern Skeleton. ***/
/* This implements the common visitor design pattern.
   Tests show it to be slightly less efficient than the
   instanceof method, but easier to use.
   Replace the R and A parameters with the desired return
   and context types.*/

public class VisitSkel
{
  public class ProgramVisitor<R,A> implements Program.Visitor<R,A>
  {
    public R visit(Ann.Absyn.Prg p, A arg)
    { /* Code For Prg Goes Here */
      for (Stm x: p.liststm_)
      { /* ... */ }
      return null;
    }
  }
  public class StmVisitor<R,A> implements Stm.Visitor<R,A>
  {
    public R visit(Ann.Absyn.SAssign p, A arg)
    { /* Code For SAssign Goes Here */
      //p.ident_;
      p.type_.accept(new TypeVisitor<R,A>(), arg);
      p.exp_.accept(new ExpVisitor<R,A>(), arg);
      return null;
    }    public R visit(Ann.Absyn.SPrint p, A arg)
    { /* Code For SPrint Goes Here */
      p.type_.accept(new TypeVisitor<R,A>(), arg);
      p.exp_.accept(new ExpVisitor<R,A>(), arg);
      return null;
    }
  }
  public class ExpVisitor<R,A> implements Exp.Visitor<R,A>
  {
    public R visit(Ann.Absyn.EInt p, A arg)
    { /* Code For EInt Goes Here */
      //p.integer_;
      return null;
    }    public R visit(Ann.Absyn.EDouble p, A arg)
    { /* Code For EDouble Goes Here */
      //p.double_;
      return null;
    }    public R visit(Ann.Absyn.EVar p, A arg)
    { /* Code For EVar Goes Here */
      p.type_.accept(new TypeVisitor<R,A>(), arg);
      //p.ident_;
      return null;
    }    public R visit(Ann.Absyn.EArith p, A arg)
    { /* Code For EArith Goes Here */
      p.type_.accept(new TypeVisitor<R,A>(), arg);
      p.exp_1.accept(new ExpVisitor<R,A>(), arg);
      p.op_.accept(new OpVisitor<R,A>(), arg);
      p.exp_2.accept(new ExpVisitor<R,A>(), arg);
      return null;
    }    public R visit(Ann.Absyn.EI2D p, A arg)
    { /* Code For EI2D Goes Here */
      p.exp_.accept(new ExpVisitor<R,A>(), arg);
      return null;
    }
  }
  public class TypeVisitor<R,A> implements Type.Visitor<R,A>
  {
    public R visit(Ann.Absyn.TInt p, A arg)
    { /* Code For TInt Goes Here */
      return null;
    }    public R visit(Ann.Absyn.TDouble p, A arg)
    { /* Code For TDouble Goes Here */
      return null;
    }
  }
  public class OpVisitor<R,A> implements Op.Visitor<R,A>
  {
    public R visit(Ann.Absyn.Times p, A arg)
    { /* Code For Times Goes Here */
      return null;
    }    public R visit(Ann.Absyn.Div p, A arg)
    { /* Code For Div Goes Here */
      return null;
    }    public R visit(Ann.Absyn.Plus p, A arg)
    { /* Code For Plus Goes Here */
      return null;
    }    public R visit(Ann.Absyn.Minus p, A arg)
    { /* Code For Minus Goes Here */
      return null;
    }
  }
}
