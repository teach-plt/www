import Mini.Absyn.*;

import java.util.HashMap;
import java.util.LinkedList;

// compiler that works with the ordinary type checker, without type annotation
// all values are assumed to be integers

public class Compiler {

    public void compile(Program p) {
	Prog prog = (Prog)p;
	Env env = new Env();

	System.out.println(".class public Foo") ;  // should take class name from file
	System.out.println(".super java/lang/Object") ;
	System.out.println("") ;
	System.out.println(".method public <init>()V") ;
	System.out.println("aload_0") ;
	System.out.println("invokenonvirtual java/lang/Object/<init>()V") ;
	System.out.println("return") ;
	System.out.println(".end method") ;
	System.out.println("") ;
	System.out.println(".method public static main([Ljava/lang/String;)V") ;
	System.out.println(".limit locals 100") ; //bogus limit
	System.out.println(".limit stack 1000") ; //bogus limit

	for (Stm s : prog.liststm_) { 
          compileStm(s, env);
	}

	System.out.println("return") ;
	System.out.println(".end method") ;

    }

    private static class Env { 
	private LinkedList<HashMap<String,Integer>> scopes;
        private Integer maxvar;

	public Env() {
	    scopes = new LinkedList<HashMap<String,Integer>>();
            maxvar = 0 ;
	    enterScope();
	}

	public Integer lookupVar(String x) {
	    for (HashMap<String,Integer> scope : scopes) {
		Integer v = scope.get(x);
		if (v != null)
		    return v;
	    }
	    throw new RuntimeException("Unknown variable " + x + " in " + scopes);
	}

	public void addVar(String x) {
	    scopes.getFirst().put(x,maxvar);
            maxvar++;
	}

	public void enterScope() {
	    scopes.addFirst(new HashMap<String,Integer>());
	}

	public void leaveScope() {
	    scopes.removeFirst();
	}
    }

    private Env eenv = new Env() ;

    private void compileStm(Stm st, Object arg) {
	st.accept(new StmCompiler(), arg);
    }

    private class StmCompiler implements Stm.Visitor<Object,Object> {
	public Object visit(Mini.Absyn.SDecl p, Object arg) {
	    eenv.addVar(p.ident_);
	    return null;
	}

	public Object visit(Mini.Absyn.SAss p, Object arg) {
	    compileExp(p.exp_, arg);
            System.out.println("istore " + eenv.lookupVar(p.ident_));
	    return null;
	}

	public Object visit(Mini.Absyn.SBlock p, Object arg) {
            Integer oldmax = eenv.maxvar ;
	    eenv.enterScope();
	    for (Stm st : p.liststm_) {
		compileStm(st, arg);
	    }
	    eenv.leaveScope();
            eenv.maxvar = oldmax ;
	    return null;
	}

	public Object visit(Mini.Absyn.SPrint p, Object arg) {
	    compileExp(p.exp_, arg);
	    System.out.println("invokestatic Runtime/printInt(I)V") ;
	    return null;
	}
    }

    private Object compileExp(Exp e, Object arg) {
	return e.accept(new ExpCompiler(), arg);
    }

    private class ExpCompiler implements Exp.Visitor<Object,Object> {

	public Object visit(Mini.Absyn.EVar p, Object arg) {
	    System.out.println("iload " + eenv.lookupVar(p.ident_));
            return null ;
	}

	public Object visit(Mini.Absyn.EInt p, Object arg) {
	    System.out.println("bipush " + p.integer_);
            return null ;
	}

	public Object visit(Mini.Absyn.EDouble p, Object arg) {
	    System.out.println("ldc2_w " + p.double_);
            return null ;
	}

	public Object visit(Mini.Absyn.EAdd p, Object arg) {
	    compileExp(p.exp_1, arg) ;
	    p.exp_2.accept(this, arg) ;
	    System.out.println("iadd");
            return null ;
	}

	// used in the type annotating version if ETyped is in Absyn
	public Object visit(Mini.Absyn.ETyped p, Object arg) {
	    compileExp(p.exp_, arg) ;
            return null ;
	}
    }

}
