package Code;
import Code.Absyn.*;
/*** BNFC-Generated Visitor Design Pattern Skeleton. ***/
/* This implements the common visitor design pattern.
   Tests show it to be slightly less efficient than the
   instanceof method, but easier to use.
   Replace the R and A parameters with the desired return
   and context types.*/

public class VisitSkel
{
  public class InsVisitor<R,A> implements Ins.Visitor<R,A>
  {
    public R visit(Code.Absyn.ILoad p, A arg)
    { /* Code For ILoad Goes Here */
      //p.integer_;
      return null;
    }    public R visit(Code.Absyn.IStore p, A arg)
    { /* Code For IStore Goes Here */
      //p.integer_;
      return null;
    }    public R visit(Code.Absyn.IAdd p, A arg)
    { /* Code For IAdd Goes Here */
      return null;
    }    public R visit(Code.Absyn.ISub p, A arg)
    { /* Code For ISub Goes Here */
      return null;
    }    public R visit(Code.Absyn.IMul p, A arg)
    { /* Code For IMul Goes Here */
      return null;
    }    public R visit(Code.Absyn.IDiv p, A arg)
    { /* Code For IDiv Goes Here */
      return null;
    }    public R visit(Code.Absyn.ILit p, A arg)
    { /* Code For ILit Goes Here */
      //p.integer_;
      return null;
    }    public R visit(Code.Absyn.DLoad p, A arg)
    { /* Code For DLoad Goes Here */
      //p.integer_;
      return null;
    }    public R visit(Code.Absyn.DStore p, A arg)
    { /* Code For DStore Goes Here */
      //p.integer_;
      return null;
    }    public R visit(Code.Absyn.DAdd p, A arg)
    { /* Code For DAdd Goes Here */
      return null;
    }    public R visit(Code.Absyn.DSub p, A arg)
    { /* Code For DSub Goes Here */
      return null;
    }    public R visit(Code.Absyn.DMul p, A arg)
    { /* Code For DMul Goes Here */
      return null;
    }    public R visit(Code.Absyn.DDiv p, A arg)
    { /* Code For DDiv Goes Here */
      return null;
    }    public R visit(Code.Absyn.DLit p, A arg)
    { /* Code For DLit Goes Here */
      //p.double_;
      return null;
    }    public R visit(Code.Absyn.I2D p, A arg)
    { /* Code For I2D Goes Here */
      return null;
    }    public R visit(Code.Absyn.IPrint p, A arg)
    { /* Code For IPrint Goes Here */
      return null;
    }    public R visit(Code.Absyn.DPrint p, A arg)
    { /* Code For DPrint Goes Here */
      return null;
    }
  }
}
