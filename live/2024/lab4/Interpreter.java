import java.util.*;
import fun.Absyn.*;
import fun.PrettyPrinter;

public class Interpreter {

  final Strategy strategy;

  final Map<String,Exp> sig = new TreeMap<String,Exp>();

  public Interpreter(Strategy strategy) {
    this.strategy = strategy;
  }

  public void interpret(Program p) {
    switch(p) {
      case Prog prg -> {
        DMain main = (DMain)prg.main_;
        // Make sig from listdef_
        for (var d : prg.listdef_) {
          DDef dd = (DDef)d;
          Exp e = dd.exp_;
          // [x1,x2] e --> new EAbs(x1, new EAbs(x2, e))
          var xs = dd.listident_;
          Collections.reverse(xs);
          for (String x : xs) {
            e = new EAbs(x, e);
          }
          sig.put(dd.ident_, e);
        }

        // Evaluate main_
        System.out.println(eval(new Env(), main.exp_).intValue());
      }
      default -> { throw new RuntimeException("Interpreter not implemented yet"); }
    }
  }

  // Evaluate expressions

  Value eval(Env env, Exp e) {
    switch(e) {
      case EInt p -> { return new VInt(p.integer_); }
      case EAbs p -> { return new VFun(p.ident_, p.exp_, env); }
      case EApp p -> {
        Value v1 = eval(env, p.exp_1);
        Value v2 = eval(env, p.exp_2);
        return v1.apply(v2);
      }
      case EVar p -> {
        String x = p.ident_;
        Value v = env.lookup(x);
        if (v == null) {
          Exp f = sig.get(x);
          if (f == null) throw new RuntimeException("unbound identifier " + x);
          return eval(new Env(), f);
        } else return v;
      }
      default -> { throw new RuntimeException("not yet implemented" + PrettyPrinter.print(e)); }
    }
  }

  // Values

  abstract class Value {
    abstract public Integer intValue();
    abstract public Value   apply(Value arg);
  }

  class VInt extends Value {
    final Integer i;
    VInt(Integer i) { this.i = i; }
    public Integer intValue()       { return i; }
    public Value   apply(Value arg) { throw new RuntimeException("cannot apply number"); }
  }

  class VFun extends Value {
    final String x;
    final Exp body;
    final Env env;
    VFun (String x, Exp body, Env env) { this.x = x; this.body = body; this.env = env; }
    public Integer intValue() { throw new RuntimeException("function does not have intValue()"); }
    public Value   apply (Value arg) { return eval(new Extend(x, arg, env), body); }
  }

  // Environments

  class Env {
    public Value lookup(String x) { return null; }
  }

  class Extend extends Env {
    final String x;
    final Value v;
    final Env env;
    Extend(String x, Value v, Env env) { this.x = x; this.v = v; this.env = env; }
    public Value lookup(String x) {
      if (x.equals(this.x)) return v;
      else return env.lookup(x);
    }
  }
}
