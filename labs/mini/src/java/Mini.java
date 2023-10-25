// Mini: Interpreter and compiler for MiniJS

// Programming Language Technology
// Chalmers DAT151, Gothenburg University DIT231

import java.io.File;
import java.io.FileReader;
import java.io.InputStreamReader;
import java.io.Reader;
import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Paths;

import MiniJS.Yylex;
import MiniJS.parser;
import MiniJS.PrettyPrinter;
import MiniJS.Absyn.Program;

public class Mini
{
  public static void main(String args[]) throws Exception
  {
    // Process command line arguments and read input.
    String fileName  = null;
    String fileNoExt = null;
    String baseName  = null;
    Yylex l = null;
    try {
      Reader input;
      if (args.length == 0) {
        // Process standard input
        input = new InputStreamReader(System.in);
        baseName = "<stdin>";
      } else {
        // Process file given by first argument
        fileName  = args[0];
        fileNoExt = stripSuffix(fileName);
        baseName  = stripPath(fileNoExt);
        input = new FileReader(fileName);
      }
      l = new Yylex(input);
    } catch (IOException e) {
      die ("Error: File not found: " + args[0]);
    }

    // Parse
    Program prg = null;
    try {
      System.out.println("[Parsing...]");
      prg = (new parser(l, l.getSymbolFactory())).pProgram();
    } catch (Throwable e) {
      die ("At line "      + String.valueOf(l.line_num())
           + ", near \""   + l.buff()
           + "\" :\n     " + e.getMessage());
    }
    System.out.println();
    System.out.println("Parse Successful!");
    System.out.println();
    System.out.println("[Abstract Syntax]");
    System.out.println();
    System.out.println(PrettyPrinter.show(prg));
    System.out.println();
    System.out.println("[Linearized Tree]");
    System.out.println();
    System.out.println(PrettyPrinter.print(prg));

    // Interpret
    System.out.println("[Running...]");
    (new Interpreter()).interpret(prg);

    // Type check
    System.out.println("[Type checking...]");
    Ann.Absyn.Program aprg = TypeChecker.typeCheck(prg);

    // Compile
    System.out.println("[Compiling...]");
    String text = (new Compiler()).compile(baseName, aprg);
    System.out.println(text);

    if (fileName != null) {

      // Write to .j file
      String jFile = fileNoExt + ".j";
      Files.write (Paths.get(jFile), text.getBytes());

      // Call jasmin to create .class file and wait for it to finish.
      System.out.println("[Calling jasmin...]");
      int exitValue = (new ProcessBuilder("jasmin", jFile))
        .inheritIO().start().waitFor();
      if (exitValue != 0)
        die ("Execution failed: " + "jasmin " + jFile);

      // Call java and let it run.
      System.out.println("[Calling java...]");
      (new ProcessBuilder("java", baseName)).inheritIO().start();
    }
  }

  // Utilities
  ///////////////////////////////////////////////////////////////////////////

  // Terminate with error message.
  private static void die (String msg) {
    System.err.println(msg);
    System.exit(1);
  }

  // Remove path from a file name.
  private static String stripPath(String name) {
    return new File(name).getName();
  }

  // Retain just the path from a file name.
  // Returns "." if there was no path.
  private static String stripFileName(String name) {
    String dir = new File(name).getParent();
    if (dir == null) dir = ".";
    return dir;
  }

  // Remove extension from a file name (keep the path).
  private static String stripSuffix(String filename) {
    int divider = filename.lastIndexOf('.');
    if (divider <= 0) return filename;
    else return filename.substring(0, divider);
  }
}
