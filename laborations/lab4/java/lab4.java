import Fun.*;
import Fun.Absyn.*;
import java.io.*;

public class lab4 {

    public static void usage() {
        System.err.println("Usage: lab4 <SourceFile>");
        System.exit(1);
    }

    public static void main(String args[]) {
        String file = null;
        Strategy strategy = Strategy.CallByValue;  // call-by-value is default strategy
        for (String arg : args) {
            switch (arg) {
                case "-n":
                    strategy = Strategy.CallByName;
                    break;

                case "-v":
                    strategy = Strategy.CallByValue;
                    break;

                default:
                    file = arg;
                    break;
            }
        }
        if (file == null) usage();

        Yylex l = null;
        try {
            l = new Yylex(new FileReader(file));
            parser p = new parser(l);
            Program prg = p.pProgram();
            new Interpreter(strategy).interpret(prg);
        } catch (RuntimeException e) {
            System.out.println("INTERPRETER ERROR");
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
