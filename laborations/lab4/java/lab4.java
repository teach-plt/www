import java_cup.runtime.*;
import Fun.*;
import Fun.Absyn.*;
import java.io.*;
import java.util.*;

public class lab4 {

    public static void usage() {
        System.err.println("Usage: lab4 [-n|-v] <SourceFile>");
        System.exit(1);
    }
    public static void main(String args[]) {
        String file = null;
        boolean strategy = false;  // call-by-value is default strategy

        // Parse command line
        for (String arg: Arrays.asList(args)) {
          if (arg.equals("-n"))      strategy = true;
          else if (arg.equals("-v")) strategy = false;
          else if (file != null) usage();
          else file = arg;
        }
        if (file == null) usage();

        Yylex l = null;
        try {
            l = new Yylex(new FileReader(file));
            parser p = new parser(l);
            Program parse_tree = p.pProgram();
            new Interpreter(strategy).interpret(parse_tree);

        } catch (RuntimeException e) {
            //            System.out.println("RUNTIME ERROR");
            System.err.println(e.toString());
            System.exit(-1);
        } catch (IOException e) {
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
