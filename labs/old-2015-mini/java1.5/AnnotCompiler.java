import Mini.Absyn.*;

import java.util.HashMap;
import java.util.LinkedList;

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
            maxvar = 1 ;
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

	public void addVar(String x, AnnotatingTypeChecker.TypeCode tc) {
	    scopes.getFirst().put(x,maxvar);
            maxvar++;
	    if (tc==AnnotatingTypeChecker.TypeCode.DOUBLE) maxvar++ ;   // increment once more if double
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
	    eenv.addVar(p.ident_,AnnotatingTypeChecker.typeCode(p.type_));
	    return null;
	}

	public Object visit(Mini.Absyn.SAss p, Object arg) {
	    AnnotatingTypeChecker.TypeCode tc = compileExp(p.exp_, AnnotatingTypeChecker.TypeCode.INT) ;
	    if (tc == AnnotatingTypeChecker.TypeCode.DOUBLE)
		System.out.println("dstore " + eenv.lookupVar(p.ident_));
	    else
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
	    AnnotatingTypeChecker.TypeCode tc = compileExp(p.exp_, AnnotatingTypeChecker.TypeCode.INT);
	    if (tc == AnnotatingTypeChecker.TypeCode.DOUBLE)
		System.out.println("invokestatic Runtime/printDouble(D)V") ;
	    else
		System.out.println("invokestatic Runtime/printInt(I)V") ;
	    return null;
	}
    }
    
    private AnnotatingTypeChecker.TypeCode compileExp(Exp e, AnnotatingTypeChecker.TypeCode arg) {
	return e.accept(new ExpCompiler(), arg);
    }

    private class ExpCompiler implements Exp.Visitor<AnnotatingTypeChecker.TypeCode,AnnotatingTypeChecker.TypeCode> {

	public AnnotatingTypeChecker.TypeCode visit(Mini.Absyn.EVar p, AnnotatingTypeChecker.TypeCode arg) {
            if (arg == AnnotatingTypeChecker.TypeCode.DOUBLE)
		System.out.println("dload " + eenv.lookupVar(p.ident_));
	    else
		System.out.println("iload " + eenv.lookupVar(p.ident_));
            return arg ;
	}

	public AnnotatingTypeChecker.TypeCode visit(Mini.Absyn.EInt p, AnnotatingTypeChecker.TypeCode arg) {
	    System.out.println("bipush " + p.integer_);
            return arg ;
	}

	public AnnotatingTypeChecker.TypeCode visit(Mini.Absyn.EDouble p, AnnotatingTypeChecker.TypeCode arg) {
	    System.out.println("ldc2_w " + p.double_);
            return arg ;
	}

	public AnnotatingTypeChecker.TypeCode visit(Mini.Absyn.EAdd p, AnnotatingTypeChecker.TypeCode arg) {
	    compileExp(p.exp_1, arg) ;
	    compileExp(p.exp_2, arg) ;
            if (arg == AnnotatingTypeChecker.TypeCode.DOUBLE)
		System.out.println("dadd");
	    else
		System.out.println("iadd");
            return arg ;
	}

	// needed in the type annotating version if ETyped is in Absyn
	public AnnotatingTypeChecker.TypeCode visit(Mini.Absyn.ETyped p, AnnotatingTypeChecker.TypeCode arg) {
	    AnnotatingTypeChecker.TypeCode tc = AnnotatingTypeChecker.typeCode(p.type_) ;
	    compileExp(p.exp_, tc) ;
            return tc ;
	}

    }

}
