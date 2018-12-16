import java.util.*;
import Fun.Absyn.*;

public class Interpreter {

  final Strategy strategy;

  public Interpreter(Strategy strategy) {
    this.strategy = strategy;
  }

  public void interpret(Program p) {
    throw new RuntimeException("Interpreter not implemented yet");
  }

}
