import Mini.Absyn.*;

import java.util.HashMap;
import java.util.LinkedList;

public class Interpreter {

    public void interpret(Program p) {
	Prog prog = (Prog)p;
	Env env = new Env();
	for (Stm s : prog.liststm_) {
	    execStm(s, env);
	}
    }

    private static abstract class Value {
	public boolean isInt() { return false; }
	public Integer getInt() { 
	    throw new RuntimeException(this + " is not an integer."); 
	}
	public Double getDouble() { 
	    throw new RuntimeException(this + " is not a double."); 
	}

	public static class Undefined extends Value {
	    public Undefined() {}
	    public String toString() { return "undefined"; }
	}
	public static class IntValue extends Value {
	    private Integer i;
	    public IntValue(Integer i) { this.i = i; }
	    public boolean isInt() { return true; }
	    public Integer getInt() { return i; }
	    public String toString() { return i.toString(); }
	}
	public static class DoubleValue extends Value {
	    private Double d;
	    public DoubleValue(Double d) { this.d = d; }
	    public Double getDouble() { return d; }
	    public String toString() { return d.toString(); }
	}
    }

    private static class Env { 
	private LinkedList<HashMap<String,Value>> scopes;

	public Env() {
	    scopes = new LinkedList<HashMap<String,Value>>();
	    enterScope();
	}

	public Value lookupVar(String x) {
	    for (HashMap<String,Value> scope : scopes) {
		Value v = scope.get(x);
		if (v != null)
		    return v;
	    }
	    throw new RuntimeException("Unknown variable " + x + " in " + scopes);
	}

	public void addVar(String x) {
	    scopes.getFirst().put(x,new Value.Undefined());
	}

	public void setVar(String x, Value v) {
	    for (HashMap<String,Value> scope : scopes) {
		if (scope.containsKey(x)) {
		    scope.put(x,v);
		    return;
		}
	    }
	}

	public void enterScope() {
	    scopes.addFirst(new HashMap<String,Value>());
	}

	public void leaveScope() {
	    scopes.removeFirst();
	}
    }

    private void execStm(Stm st, Env env) {
	st.accept(new StmExecuter(), env);
    }

    private class StmExecuter implements Stm.Visitor<Object,Env> {
	public Object visit(Mini.Absyn.SDecl p, Env env) {
	    env.addVar(p.ident_);
	    return null;
	}

	public Object visit(Mini.Absyn.SAss p, Env env) {
	    env.setVar(p.ident_, evalExp(p.exp_, env));
	    return null;
	}

	public Object visit(Mini.Absyn.SBlock p, Env env) {
	    env.enterScope();
	    for (Stm st : p.liststm_) {
		execStm(st, env);
	    }
	    env.leaveScope();
	    return null;
	}

	public Object visit(Mini.Absyn.SPrint p, Env env) {
	    Value v = evalExp(p.exp_, env);
	    System.err.println(v.toString());
	    return null;
	}
    }

    private Value evalExp(Exp e, Env env) {
	return e.accept(new ExpEvaluator(), env);
    }

    private class ExpEvaluator implements Exp.Visitor<Value,Env> {

	public Value visit(Mini.Absyn.EVar p, Env env) {
	    return env.lookupVar(p.ident_);
	}

	public Value visit(Mini.Absyn.EInt p, Env env) {
	    return new Value.IntValue(p.integer_);
	}
	public Value visit(Mini.Absyn.EDouble p, Env env) {
	    return new Value.DoubleValue(p.double_);
	}
	public Value visit(Mini.Absyn.EAdd p, Env env) {
	    Value v1 = p.exp_1.accept(this, env);
	    Value v2 = p.exp_2.accept(this, env);
	    if (v1.isInt()) {
		return new Value.IntValue(v1.getInt() + v2.getInt());
	    } else {
		return new Value.DoubleValue(v1.getDouble() + v2.getDouble());
	    }
	}

	// needed in the type annotating version if ETyped is in Absyn
	public Value visit(Mini.Absyn.ETyped p, Env env) {
	    return p.exp_.accept(this,env) ;
	}
    }

}
