import Mini.Absyn.*;
import Mini.PrettyPrinter;

import java.util.HashMap;
import java.util.LinkedList;


public class TypeChecker {

    private static enum TypeCode { 
        INT    { public String toString() { return "int";    } }, 
        DOUBLE { public String toString() { return "double"; } }
    }

    private static class Env { 
	private LinkedList<HashMap<String,TypeCode>> scopes;

	public Env() {
	    scopes = new LinkedList<HashMap<String,TypeCode>>();
	    enterScope();
	}

	public TypeCode lookupVar(String x) {
	    for (HashMap<String,TypeCode> scope : scopes) {
		TypeCode t = scope.get(x);
		if (t != null)
		    return t;
	    }
	    throw new TypeException("Unknown variable " + x + ".");
	}

	public void addVar(String x, TypeCode t) {
	    if (scopes.getFirst().containsKey(x))
		throw new TypeException("Variable " + x 
					+ " is already declared in this scope.");
	    scopes.getFirst().put(x,t);
	}

	public void enterScope() {
	    scopes.addFirst(new HashMap<String,TypeCode>());
	}

	public void leaveScope() {
	    scopes.removeFirst();
	}
    }

    public void typecheck(Program p) {
	Prog prog = (Prog)p;
	Env env = new Env();
	for (Stm s : prog.liststm_) {
	    checkStm(s, env);
	}
    }

    private void checkStm(Stm st, Env env) {
	st.accept(new StmChecker(), env);
    }

    private class StmChecker implements Stm.Visitor<Object,Env> {
	public Object visit(SDecl p, Env env) {
	    env.addVar(p.ident_, typeCode(p.type_));
	    return null;
	}

	public Object visit(SAss p, Env env) {
	    TypeCode t = env.lookupVar(p.ident_);
	    checkExp(p.exp_, t, env);
	    return null;
	}

	public Object visit(SBlock p, Env env) {
	    env.enterScope();
	    for (Stm st : p.liststm_) {
		checkStm(st, env);
	    }
	    env.leaveScope();
	    return null;
	}

	public Object visit(SPrint p, Env env) {
	    // we don't care what the type is, just that there is one
	    inferExp(p.exp_, env);
	    return null;
	}
    }

    private void checkExp(Exp e, TypeCode t, Env env) {
	TypeCode et = inferExp(e,env);
	if (et != t) {
	    throw new TypeException(PrettyPrinter.print(e) 
				    + " has type " + et 
				    + " expected " + t);
	}
    }

    private TypeCode inferExp(Exp e, Env env) {
	return e.accept(new TypeInferrer(), env);
    }

    private class TypeInferrer implements Exp.Visitor<TypeCode,Env> {

	public TypeCode visit(EVar p, Env env) {
	    return env.lookupVar(p.ident_);
	}

	public TypeCode visit(EInt p, Env env) {
	    return TypeCode.INT;
	}
	public TypeCode visit(EDouble p, Env env) {
	    return TypeCode.DOUBLE;
	}
	public TypeCode visit(EAdd p, Env env) {
	    TypeCode t1 = p.exp_1.accept(this, env);
	    TypeCode t2 = p.exp_2.accept(this, env);

	    if (t1 != t2) {
		throw new TypeException(PrettyPrinter.print(p.exp_1) + 
					" has type " + t1
					+ " but " + PrettyPrinter.print(p.exp_1)
					+ " has type " + t2);
	    }

	    return t1;
	}

	// needed in the type annotating version if ETyped is in Absyn
	public TypeCode visit(ETyped p, Env env) {
	    return typeCode(p.type_) ;
	}

    }

    private TypeCode typeCode(Type t) {
	return t.accept(new TypeCoder(), null);
    }

    private class TypeCoder implements Type.Visitor<TypeCode,Object> {
	public TypeCode visit(TInt t, Object arg) {
	    return TypeCode.INT;
	}
	public TypeCode visit(TDouble t, Object arg) {
	    return TypeCode.DOUBLE;
	}
    }

}
