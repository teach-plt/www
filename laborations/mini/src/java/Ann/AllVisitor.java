package Ann;

import Ann.Absyn.*;

/** BNFC-Generated All Visitor */
public interface AllVisitor<R,A> extends
  Ann.Absyn.Program.Visitor<R,A>,
  Ann.Absyn.Stm.Visitor<R,A>,
  Ann.Absyn.Exp.Visitor<R,A>,
  Ann.Absyn.Type.Visitor<R,A>,
  Ann.Absyn.Op.Visitor<R,A>
{}
