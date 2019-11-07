package MiniJS;
import MiniJS.Absyn.*;
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
    public R visit(MiniJS.Absyn.Prg p, A arg)
    { /* Code For Prg Goes Here */
      for (Stm x: p.liststm_)
      { /* ... */ }
      return null;
    }
  }
  public class StmVisitor<R,A> implements Stm.Visitor<R,A>
  {
    public R visit(MiniJS.Absyn.SAssign p, A arg)
    { /* Code For SAssign Goes Here */
      //p.ident_;
      p.exp_.accept(new ExpVisitor<R,A>(), arg);
      return null;
    }    public R visit(MiniJS.Absyn.SPrint p, A arg)
    { /* Code For SPrint Goes Here */
      p.exp_.accept(new ExpVisitor<R,A>(), arg);
      return null;
    }
  }
  public class ExpVisitor<R,A> implements Exp.Visitor<R,A>
  {
    public R visit(MiniJS.Absyn.EVar p, A arg)
    { /* Code For EVar Goes Here */
      //p.ident_;
      return null;
    }    public R visit(MiniJS.Absyn.EInt p, A arg)
    { /* Code For EInt Goes Here */
      //p.integer_;
      return null;
    }    public R visit(MiniJS.Absyn.EDouble p, A arg)
    { /* Code For EDouble Goes Here */
      //p.double_;
      return null;
    }        public R visit(MiniJS.Absyn.ETimes p, A arg)
    { /* Code For ETimes Goes Here */
      p.exp_1.accept(new ExpVisitor<R,A>(), arg);
      p.exp_2.accept(new ExpVisitor<R,A>(), arg);
      return null;
    }    public R visit(MiniJS.Absyn.EDiv p, A arg)
    { /* Code For EDiv Goes Here */
      p.exp_1.accept(new ExpVisitor<R,A>(), arg);
      p.exp_2.accept(new ExpVisitor<R,A>(), arg);
      return null;
    }        public R visit(MiniJS.Absyn.EPlus p, A arg)
    { /* Code For EPlus Goes Here */
      p.exp_1.accept(new ExpVisitor<R,A>(), arg);
      p.exp_2.accept(new ExpVisitor<R,A>(), arg);
      return null;
    }    public R visit(MiniJS.Absyn.EMinus p, A arg)
    { /* Code For EMinus Goes Here */
      p.exp_1.accept(new ExpVisitor<R,A>(), arg);
      p.exp_2.accept(new ExpVisitor<R,A>(), arg);
      return null;
    }
  }
}
