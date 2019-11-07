// Interpreter for MiniJS untyped syntax (MiniJS).

// Programming Language Technology
// Chalmers DAT151, Gothenburg University DIT231

import MiniJS.Absyn.*;

public class Interpreter
{
  public Interpreter() {}

  // Interpreter entry point

  public void interpret(MiniJS.Absyn.Program prg) {
    prg.accept(new ExecPrg(), null);
  }

  // Execute the program

  public class ExecPrg implements Program.Visitor<Void,Void>
  {
    public Void visit(MiniJS.Absyn.Prg prg, Void arg)
    {
      Environment env  = new Empty();
      ExecStm     exec = new ExecStm();
      for (Stm s: prg.liststm_)
        env = s.accept(exec, env);
      return null;
    }
  }

  // Execute statements

  public class ExecStm implements Stm.Visitor<Environment,Environment>
  {
    public Environment visit(MiniJS.Absyn.SAssign p, Environment env)
    {
      Value v = p.exp_.accept(new EvalExp(), env);
      return env.insert(p.ident_, v);
    }
    public Environment visit(MiniJS.Absyn.SPrint p, Environment env)
    {
      Value v = p.exp_.accept(new EvalExp(), env);
      v.print();
      return env;
    }
  }

  // Evaluate expressions

  public class EvalExp implements Exp.Visitor<Value,Environment>
  {
    public Value visit(MiniJS.Absyn.EVar p, Environment env)
    {
      return env.lookup(p.ident_);
    }
    public Value visit(MiniJS.Absyn.EInt p, Environment env)
    {
      return new VInt (p.integer_);
    }
    public Value visit(MiniJS.Absyn.EDouble p, Environment env)
    {
      return new VDouble (p.double_);
    }
    public Value visit(MiniJS.Absyn.ETimes p, Environment env)
    {
      Value v1 = p.exp_1.accept(new EvalExp(), env);
      Value v2 = p.exp_2.accept(new EvalExp(), env);
      return v1.mul(v2);
    }
    public Value visit(MiniJS.Absyn.EDiv p, Environment env)
    {
      Value v1 = p.exp_1.accept(new EvalExp(), env);
      Value v2 = p.exp_2.accept(new EvalExp(), env);
      return v1.div(v2);
    }
    public Value visit(MiniJS.Absyn.EPlus p, Environment env)
    {
      Value v1 = p.exp_1.accept(new EvalExp(), env);
      Value v2 = p.exp_2.accept(new EvalExp(), env);
      return v1.add(v2);
    }
    public Value visit(MiniJS.Absyn.EMinus p, Environment env)
    {
      Value v1 = p.exp_1.accept(new EvalExp(), env);
      Value v2 = p.exp_2.accept(new EvalExp(), env);
      return v1.sub(v2);
    }
  }

  // Environments as linked lists

  abstract public class Environment {
    abstract public Value lookup(String x);
    public Environment insert (String x, Value v) {
      return new Extend(x, v, this); // no garbage collection
    }
  }

  public class Empty extends Environment {
    public Empty() {}
    public Value lookup (String x) {
      throw new RuntimeException("unbound variable " + x);
    }
  }

  public class Extend extends Environment {
    final String x;
    final Value  v;
    final Environment env;
    public Extend(String x, Value v, Environment env) {
      this.x = x;
      this.v = v;
      this.env = env;
    }
    public Value lookup (String y) {
      if (y.equals(x)) return v;
      else return env.lookup(y);
    }
  }

  // Values

  abstract public class Value {
    abstract public Integer intValue ();
    abstract public Double doubleValue ();
    abstract public Value add (Value v);
    abstract public Value sub (Value v);
    abstract public Value mul (Value v);
    abstract public Value div (Value v);
    abstract public void print();
  }

  public class VInt extends Value {
    final Integer i;
    public VInt(Integer i) { this.i = i; }
    public Integer intValue() { return i; }
    public Double doubleValue() { return new Double(i); }
    public Value add (Value v) {
      if (v instanceof VInt) return new VInt(i + v.intValue());
      else return new VDouble(i + v.doubleValue());
    }
    public Value sub (Value v) {
      if (v instanceof VInt) return new VInt(i - v.intValue());
      else return new VDouble(i - v.doubleValue());
    }
    public Value mul (Value v) {
      if (v instanceof VInt) return new VInt(i * v.intValue());
      else return new VDouble(i * v.doubleValue());
    }
    public Value div (Value v) {
      if (v instanceof VInt) return new VInt(i / v.intValue());
      else return new VDouble(i / v.doubleValue());
    }
    public void print() { System.out.println(i); }
  }

  public class VDouble extends Value {
    final Double d;
    public VDouble(Double d) { this.d = d; }
    public Double doubleValue() { return d; }
    public Integer intValue() {
      throw new RuntimeException("cannot convert Double value to Integer value");
    }
    public Value add (Value v) { return new VDouble(d + v.doubleValue()); }
    public Value sub (Value v) { return new VDouble(d - v.doubleValue()); }
    public Value mul (Value v) { return new VDouble(d * v.doubleValue()); }
    public Value div (Value v) { return new VDouble(d / v.doubleValue()); }
    public void print() { System.out.println(d); }
  }

}
