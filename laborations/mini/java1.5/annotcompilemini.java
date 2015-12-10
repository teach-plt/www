import Mini.*;

public class compilemini {

    public static void main(String args[]) {
	if (args.length != 1) {
	    System.err.println("Usage: compilemini <SourceFile>");
	    System.exit(1);	
	}

	Mini.Yylex l = null;
	try {
	    l = new Mini.Yylex(new java.io.FileReader(args[0]));
	    Mini.parser p = new Mini.parser(l);
	    Mini.Absyn.Program parse_tree = p.pProgram();

	    System.err.println("[Parsed Tree]");
	    System.err.println(PrettyPrinter.show(parse_tree));

	    new AnnotatingTypeChecker().typecheck(parse_tree);

	    System.err.println("[Annotated Tree]");
	    System.err.println(PrettyPrinter.show(parse_tree));
	    System.err.println(PrettyPrinter.print(parse_tree));

	    new Compiler().compile(parse_tree);
	} catch (TypeException e) {
	    System.out.println("TYPE ERROR");
	    System.err.println(e.toString());
	    System.exit(1);
	} catch (RuntimeException e) {
	    System.out.println("RUNTIME ERROR");
	    System.err.println(e.toString());
	    System.exit(1);
	} catch (java.io.IOException e) {
	    System.err.println(e.toString());
	    System.exit(1);
	} catch (Throwable e) {
	    System.out.println("SYNTAX ERROR");
	    System.out.println("At line " + String.valueOf(l.line_num()) 
			       + ", near \"" + l.buff() + "\" :");
	    System.out.println("     " + e.getMessage());
	    e.printStackTrace();
	    System.exit(1);
	}
    }
    
}
