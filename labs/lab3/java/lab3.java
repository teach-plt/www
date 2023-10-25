// PLT lab 3 in Java:
// Annotating type checker for C-- and compiler to symbolic JVM assembly.

import java.io.*;
import java.util.*;
import java_cup.runtime.*;
import cmm.*;
import cmm.Absyn.*;

public class lab3 {

  public static void main(String args[]) {

    // Ensure that we are called with a single argument.
    if (args.length != 1) {
      System.err.println("Usage: lab3 <SourceFile>");
      System.exit(1);
    }

    String srcFile   = args[0];                // Ex: path/to/file.cc
    String fileCore  = stripSuffix(srcFile);   // Ex: path/to/file
    String dir       = stripFileName(srcFile); // Ex: path/to  or "."
    String className = stripPath(fileCore);    // Ex:         file
    String jFile     = fileCore + ".j";        // Ex: path/to/file.j

    Yylex  l = null;
    try {

      // Parse
      l = new Yylex(new FileReader(srcFile)); // throws FileNotFoundException
      parser p = new parser(l);
      cmm.Absyn.Program parseTree = p.pProgram();

      // Type check
      cmm.Absyn.Program typedTree = new TypeChecker().typecheck(parseTree);

      // Compile
      String jtext = new Compiler().compile(className, typedTree);

      // Write .j file to same directory where source file was.
      PrintWriter writer = new PrintWriter(jFile);
      writer.print(jtext);
      writer.close();

      // Call jasmin to create .class file.
      callJasmin(Arrays.asList("-d", dir, jFile));
    }
    catch (TypeException e) {
      System.out.println("TYPE ERROR");
      System.err.println(e.toString());
      System.exit(1);
    }
    catch (RuntimeException e) {
      System.err.println(e.toString());
      System.exit(-1);
    }
    catch (IOException e) {
      System.err.println(e.toString());
      System.exit(1);
    }
    catch (Throwable e) {
      System.out.println("SYNTAX ERROR");
      System.out.println("At line " + String.valueOf(l.line_num())
                 + ", near \"" + l.buff() + "\" :");
      System.out.println("     " + e.getMessage());
      e.printStackTrace();
      System.exit(1);
    }
  }

  // Invoke the external assembler `jasmin` with the given args, waiting for it to finish.
  static void callJasmin(List<String> args) throws IOException, InterruptedException {

    // Path to this class file.
    String myself     = lab3.class.getResource("lab3.class").getPath();
    // Directory from which this class was started.
    File   directory  = new File(myself).getParentFile();
    // jasmin.jar should be in the same directory.
    String jasminPath = new File(directory, "jasmin.jar").toString();
    List<String> jasminCall = new ArrayList<String>(Arrays.asList( "java", "-jar", jasminPath ));

    // Note: If `jasmin` is in your PATH, you can replace the above code by the simpler:
    // List<String> jasminCall = new ArrayList<String>(Arrays.asList("jasmin"));

    jasminCall.addAll(args);
    Process process = new ProcessBuilder(jasminCall).inheritIO().start();
    int exitValue = process.waitFor();
    if (exitValue != 0) {
      System.err.println("Execution failed: " + String.join(" ", jasminCall));
      System.exit(1);
    }
  }

  // Utilities
  ///////////////////////////////////////////////////////////////////////////

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
