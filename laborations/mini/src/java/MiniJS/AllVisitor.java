package MiniJS;

import MiniJS.Absyn.*;

/** BNFC-Generated All Visitor */
public interface AllVisitor<R,A> extends
  MiniJS.Absyn.Program.Visitor<R,A>,
  MiniJS.Absyn.Stm.Visitor<R,A>,
  MiniJS.Absyn.Exp.Visitor<R,A>
{}
