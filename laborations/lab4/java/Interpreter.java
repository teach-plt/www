import java.util.*;
import Fun.Absyn.*;

public class Interpreter {

  // Signature
  final Map<String,Exp> sig  = new TreeMap();

  // Strategy
  final boolean strategy;

  // Control debug printing.
  final boolean debug = false;

  public Interpreter (boolean strategy) {
    this.strategy = strategy;
  }

  public void interpret(Program p) {
    System.out.println(p.accept(new ProgramVisitor(), null).intValue());
  }

  public class ProgramVisitor implements Program.Visitor<Value,Void>
  {
    public Value visit(Fun.Absyn.Prog p, Void arg)
    {
      // build signature
      for (Def d: p.listdef_) d.accept(new DefVisitor(), null);
      // execute main expression
      return p.main_.accept(new MainVisitor(), null);
    }
  }

  public class MainVisitor implements Main.Visitor<Value,Void>
  {
    public Value visit(Fun.Absyn.DMain p, Void arg)
    {
      if (debug) {
        for (String key : sig.keySet())
          System.out.println ("Check: Found key " + key + " in the signature");
        System.out.println("Evaluating main expression: " +
          Fun.PrettyPrinter.print(p.exp_));
      }
      return p.exp_.accept(new EvalVisitor(), new Empty());
    }
  }

  // visit defs only to build the signature.
  public class DefVisitor implements Def.Visitor<Void,Void>
  {
    public Void visit(Fun.Absyn.DDef p, Void arg)
    {
      // abstract over arguments from right to left
      Exp e = p.exp_;

      Collections.reverse(p.listident_);
      for (String x: p.listident_) e = new EAbs(x, e);

      // Add to signature
      sig.put(p.ident_, e);

      if (debug) {
        if (sig.get(p.ident_) != null)
          System.out.println("Added " + p.ident_ + " to the signature.");
        else System.out.println("Failed to add " + p.ident_ + " to the signature.");
      }
      return null;
    }
  }

  public class EvalVisitor implements Exp.Visitor<Value,Environment>
  {
    // variable
    public Value visit(Fun.Absyn.EVar p, Environment env)
    {
      if (debug) {
        for (String key : sig.keySet())
          System.out.println ("EVar: Found key " + key + " in the signature");
        System.out.println ("Resolving variable " + p.ident_);
      }

      // Look first in the local environment
      Value v = env.lookup(p.ident_);
      if (v != null) {
        if (debug) System.out.println ("Found " + p.ident_ + " in the environment");
        return v;
      }

      // Look then in the global signature
      if (debug) {
        if (sig.isEmpty()) System.out.println ("Signature is empty!");
        else System.out.println ("Signature is not empty!");
        for (String key : sig.keySet())
          System.out.println ("Found key " + key + " in the signature");
      }

      Exp e = sig.get(p.ident_);
      if (e != null) {
        if (debug) System.out.println ("Found " + p.ident_ + " in the signature");
        return e.accept(this, new Empty());
      }
      // not found?
      else throw new RuntimeException("unbound variable " + p.ident_);
    }

    // literal
    public Value visit(Fun.Absyn.EInt p, Environment env)
    {
      return new VInt(p.integer_);
    }

    // lambda
    public Value visit(Fun.Absyn.EAbs p, Environment env)
    {
      return new VClos (p.ident_, p.exp_, env);
    }

    // application
    public Value visit(Fun.Absyn.EApp p, Environment env)
    {
      if (debug) {
        for (String key : sig.keySet())
          System.out.println ("EApp: Found key " + key + " in the signature");
      }
      if (strategy) todo("call-by-name");
      // Evaluate the function
      Value vfun = p.exp_1.accept (this, env);
      // Evaluate the argument
      Value varg = p.exp_2.accept (this, env);
      return vfun.apply(varg);
    }

    // plus
    public Value visit(Fun.Absyn.EAdd p, Environment env)
    {
      // p.exp_1.accept(new EvalVisitor(), env);
      // p.exp_2.accept(new EvalVisitor(), env);
      todo("plus");
      return null;
    }

    // minus
    public Value visit(Fun.Absyn.ESub p, Environment env)
    {
      // p.exp_1.accept(new EvalVisitor(), env);
      // p.exp_2.accept(new EvalVisitor(), env);
      todo("minus");
      return null;
    }

    // less-than
    public Value visit(Fun.Absyn.ELt p, Environment env)
    {
      // p.exp_1.accept(new EvalVisitor(), env);
      // p.exp_2.accept(new EvalVisitor(), env);
      todo("less-than");
      return null;
    }

    // if
    public Value visit(Fun.Absyn.EIf p, Environment env)
    {
      // p.exp_1.accept(new EvalVisitor(), env);
      // p.exp_2.accept(new EvalVisitor(), env);
      // p.exp_3.accept(new EvalVisitor(), env);
      todo("if");
      return null;
    }
  }

  // TODOs /////////////////////////////////////////////////////////////

  public void todo(String msg) {
    throw new RuntimeException ("TODO: " + msg);
  }

  // Value /////////////////////////////////////////////////////////////

  abstract class Value {
    abstract public int   intValue();
    abstract public Value apply(Value v);
  }

  // Numeric values

  class VInt extends Value {

    final int val;
    public VInt (int i) { val = i; }

    public int   intValue() {
      return val;
    }
    public Value apply(Value v) {
      throw new RuntimeException ("cannot apply a number");
    }
  }

  // Function values

  class VClos extends Value {
    final String x;
    final Exp body;
    final Environment env;

    public VClos (String x, Exp body, Environment env) {
      this.x = x;
      this.body = body;
      this.env = env;
    }

    public int intValue() { throw new RuntimeException ("not an integer"); }
    public Value apply (Value v) {
      return body.accept (new EvalVisitor(), new Extend(x,v,env));  // TODO add x=v to env
    }
    // TODO: call-by-name
  }

  // Environment ///////////////////////////////////////////////////////

  abstract class Environment {
    abstract Value lookup (String x);
  }

  class Empty extends Environment {
    Value lookup (String x) { return null; }
  }

  class Extend extends Environment {
    final String x;
    final Value  v;          // TODO: call-by-name
    final Environment rest;

    public Extend (String x, Value v, Environment rest) {
      this.x = x;
      this.v = v;
      this.rest = rest;
    }

    Value lookup (String y) {
      if (x.equals(y)) return v;
      else return rest.lookup(y);
    }
    // TODO: call-by-name

  }

}
