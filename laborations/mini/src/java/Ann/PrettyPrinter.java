package Ann;
import Ann.Absyn.*;

public class PrettyPrinter
{
  //For certain applications increasing the initial size of the buffer may improve performance.
  private static final int INITIAL_BUFFER_SIZE = 128;
  private static final int INDENT_WIDTH = 2;
  //You may wish to change the parentheses used in precedence.
  private static final String _L_PAREN = new String("(");
  private static final String _R_PAREN = new String(")");
  //You may wish to change render
  private static void render(String s)
  {
    if (s.equals("{"))
    {
       buf_.append("\n");
       indent();
       buf_.append(s);
       _n_ = _n_ + INDENT_WIDTH;
       buf_.append("\n");
       indent();
    }
    else if (s.equals("(") || s.equals("["))
       buf_.append(s);
    else if (s.equals(")") || s.equals("]"))
    {
       backup();
       buf_.append(s);
       buf_.append(" ");
    }
    else if (s.equals("}"))
    {
       int t;
       _n_ = _n_ - INDENT_WIDTH;
       for(t=0; t<INDENT_WIDTH; t++) {
         backup();
       }
       buf_.append(s);
       buf_.append("\n");
       indent();
    }
    else if (s.equals(","))
    {
       backup();
       buf_.append(s);
       buf_.append(" ");
    }
    else if (s.equals(";"))
    {
       backup();
       buf_.append(s);
       buf_.append("\n");
       indent();
    }
    else if (s.equals("")) return;
    else
    {
       buf_.append(s);
       buf_.append(" ");
    }
  }


  //  print and show methods are defined for each category.
  public static String print(Ann.Absyn.Program foo)
  {
    pp(foo, 0);
    trim();
    String temp = buf_.toString();
    buf_.delete(0,buf_.length());
    return temp;
  }
  public static String show(Ann.Absyn.Program foo)
  {
    sh(foo);
    String temp = buf_.toString();
    buf_.delete(0,buf_.length());
    return temp;
  }
  public static String print(Ann.Absyn.Stm foo)
  {
    pp(foo, 0);
    trim();
    String temp = buf_.toString();
    buf_.delete(0,buf_.length());
    return temp;
  }
  public static String show(Ann.Absyn.Stm foo)
  {
    sh(foo);
    String temp = buf_.toString();
    buf_.delete(0,buf_.length());
    return temp;
  }
  public static String print(Ann.Absyn.ListStm foo)
  {
    pp(foo, 0);
    trim();
    String temp = buf_.toString();
    buf_.delete(0,buf_.length());
    return temp;
  }
  public static String show(Ann.Absyn.ListStm foo)
  {
    sh(foo);
    String temp = buf_.toString();
    buf_.delete(0,buf_.length());
    return temp;
  }
  public static String print(Ann.Absyn.Exp foo)
  {
    pp(foo, 0);
    trim();
    String temp = buf_.toString();
    buf_.delete(0,buf_.length());
    return temp;
  }
  public static String show(Ann.Absyn.Exp foo)
  {
    sh(foo);
    String temp = buf_.toString();
    buf_.delete(0,buf_.length());
    return temp;
  }
  public static String print(Ann.Absyn.Type foo)
  {
    pp(foo, 0);
    trim();
    String temp = buf_.toString();
    buf_.delete(0,buf_.length());
    return temp;
  }
  public static String show(Ann.Absyn.Type foo)
  {
    sh(foo);
    String temp = buf_.toString();
    buf_.delete(0,buf_.length());
    return temp;
  }
  public static String print(Ann.Absyn.Op foo)
  {
    pp(foo, 0);
    trim();
    String temp = buf_.toString();
    buf_.delete(0,buf_.length());
    return temp;
  }
  public static String show(Ann.Absyn.Op foo)
  {
    sh(foo);
    String temp = buf_.toString();
    buf_.delete(0,buf_.length());
    return temp;
  }
  /***   You shouldn't need to change anything beyond this point.   ***/

  private static void pp(Ann.Absyn.Program foo, int _i_)
  {
    if (foo instanceof Ann.Absyn.Prg)
    {
       Ann.Absyn.Prg _prg = (Ann.Absyn.Prg) foo;
       if (_i_ > 0) render(_L_PAREN);
       pp(_prg.liststm_, 0);
       if (_i_ > 0) render(_R_PAREN);
    }
  }

  private static void pp(Ann.Absyn.Stm foo, int _i_)
  {
    if (foo instanceof Ann.Absyn.SAssign)
    {
       Ann.Absyn.SAssign _sassign = (Ann.Absyn.SAssign) foo;
       if (_i_ > 0) render(_L_PAREN);
       pp(_sassign.ident_, 0);
       render("=");
       pp(_sassign.type_, 0);
       render("(");
       pp(_sassign.exp_, 0);
       render(")");
       if (_i_ > 0) render(_R_PAREN);
    }
    else     if (foo instanceof Ann.Absyn.SPrint)
    {
       Ann.Absyn.SPrint _sprint = (Ann.Absyn.SPrint) foo;
       if (_i_ > 0) render(_L_PAREN);
       render("console.log");
       render("(");
       pp(_sprint.type_, 0);
       render("(");
       pp(_sprint.exp_, 0);
       render(")");
       render(")");
       if (_i_ > 0) render(_R_PAREN);
    }
  }

  private static void pp(Ann.Absyn.ListStm foo, int _i_)
  {
     for (java.util.Iterator<Stm> it = foo.iterator(); it.hasNext();)
     {
       pp(it.next(), _i_);
       if (it.hasNext()) {
         render(";");
       } else {
         render(";");
       }
     }  }

  private static void pp(Ann.Absyn.Exp foo, int _i_)
  {
    if (foo instanceof Ann.Absyn.EInt)
    {
       Ann.Absyn.EInt _eint = (Ann.Absyn.EInt) foo;
       if (_i_ > 0) render(_L_PAREN);
       pp(_eint.integer_, 0);
       if (_i_ > 0) render(_R_PAREN);
    }
    else     if (foo instanceof Ann.Absyn.EDouble)
    {
       Ann.Absyn.EDouble _edouble = (Ann.Absyn.EDouble) foo;
       if (_i_ > 0) render(_L_PAREN);
       pp(_edouble.double_, 0);
       if (_i_ > 0) render(_R_PAREN);
    }
    else     if (foo instanceof Ann.Absyn.EVar)
    {
       Ann.Absyn.EVar _evar = (Ann.Absyn.EVar) foo;
       if (_i_ > 0) render(_L_PAREN);
       pp(_evar.type_, 0);
       render("(");
       pp(_evar.ident_, 0);
       render(")");
       if (_i_ > 0) render(_R_PAREN);
    }
    else     if (foo instanceof Ann.Absyn.EArith)
    {
       Ann.Absyn.EArith _earith = (Ann.Absyn.EArith) foo;
       if (_i_ > 0) render(_L_PAREN);
       pp(_earith.type_, 0);
       render("(");
       pp(_earith.exp_1, 0);
       pp(_earith.op_, 0);
       pp(_earith.exp_2, 0);
       render(")");
       if (_i_ > 0) render(_R_PAREN);
    }
    else     if (foo instanceof Ann.Absyn.EI2D)
    {
       Ann.Absyn.EI2D _ei2d = (Ann.Absyn.EI2D) foo;
       if (_i_ > 0) render(_L_PAREN);
       render("coerce");
       render("(");
       pp(_ei2d.exp_, 0);
       render(")");
       if (_i_ > 0) render(_R_PAREN);
    }
  }

  private static void pp(Ann.Absyn.Type foo, int _i_)
  {
    if (foo instanceof Ann.Absyn.TInt)
    {
       Ann.Absyn.TInt _tint = (Ann.Absyn.TInt) foo;
       if (_i_ > 0) render(_L_PAREN);
       render("Int");
       if (_i_ > 0) render(_R_PAREN);
    }
    else     if (foo instanceof Ann.Absyn.TDouble)
    {
       Ann.Absyn.TDouble _tdouble = (Ann.Absyn.TDouble) foo;
       if (_i_ > 0) render(_L_PAREN);
       render("Double");
       if (_i_ > 0) render(_R_PAREN);
    }
  }

  private static void pp(Ann.Absyn.Op foo, int _i_)
  {
    if (foo instanceof Ann.Absyn.Times)
    {
       Ann.Absyn.Times _times = (Ann.Absyn.Times) foo;
       if (_i_ > 0) render(_L_PAREN);
       render("*");
       if (_i_ > 0) render(_R_PAREN);
    }
    else     if (foo instanceof Ann.Absyn.Div)
    {
       Ann.Absyn.Div _div = (Ann.Absyn.Div) foo;
       if (_i_ > 0) render(_L_PAREN);
       render("/");
       if (_i_ > 0) render(_R_PAREN);
    }
    else     if (foo instanceof Ann.Absyn.Plus)
    {
       Ann.Absyn.Plus _plus = (Ann.Absyn.Plus) foo;
       if (_i_ > 0) render(_L_PAREN);
       render("+");
       if (_i_ > 0) render(_R_PAREN);
    }
    else     if (foo instanceof Ann.Absyn.Minus)
    {
       Ann.Absyn.Minus _minus = (Ann.Absyn.Minus) foo;
       if (_i_ > 0) render(_L_PAREN);
       render("-");
       if (_i_ > 0) render(_R_PAREN);
    }
  }


  private static void sh(Ann.Absyn.Program foo)
  {
    if (foo instanceof Ann.Absyn.Prg)
    {
       Ann.Absyn.Prg _prg = (Ann.Absyn.Prg) foo;
       render("(");
       render("Prg");
       render("[");
       sh(_prg.liststm_);
       render("]");
       render(")");
    }
  }

  private static void sh(Ann.Absyn.Stm foo)
  {
    if (foo instanceof Ann.Absyn.SAssign)
    {
       Ann.Absyn.SAssign _sassign = (Ann.Absyn.SAssign) foo;
       render("(");
       render("SAssign");
       sh(_sassign.ident_);
       sh(_sassign.type_);
       sh(_sassign.exp_);
       render(")");
    }
    if (foo instanceof Ann.Absyn.SPrint)
    {
       Ann.Absyn.SPrint _sprint = (Ann.Absyn.SPrint) foo;
       render("(");
       render("SPrint");
       sh(_sprint.type_);
       sh(_sprint.exp_);
       render(")");
    }
  }

  private static void sh(Ann.Absyn.ListStm foo)
  {
     for (java.util.Iterator<Stm> it = foo.iterator(); it.hasNext();)
     {
       sh(it.next());
       if (it.hasNext())
         render(",");
     }
  }

  private static void sh(Ann.Absyn.Exp foo)
  {
    if (foo instanceof Ann.Absyn.EInt)
    {
       Ann.Absyn.EInt _eint = (Ann.Absyn.EInt) foo;
       render("(");
       render("EInt");
       sh(_eint.integer_);
       render(")");
    }
    if (foo instanceof Ann.Absyn.EDouble)
    {
       Ann.Absyn.EDouble _edouble = (Ann.Absyn.EDouble) foo;
       render("(");
       render("EDouble");
       sh(_edouble.double_);
       render(")");
    }
    if (foo instanceof Ann.Absyn.EVar)
    {
       Ann.Absyn.EVar _evar = (Ann.Absyn.EVar) foo;
       render("(");
       render("EVar");
       sh(_evar.type_);
       sh(_evar.ident_);
       render(")");
    }
    if (foo instanceof Ann.Absyn.EArith)
    {
       Ann.Absyn.EArith _earith = (Ann.Absyn.EArith) foo;
       render("(");
       render("EArith");
       sh(_earith.type_);
       sh(_earith.exp_1);
       sh(_earith.op_);
       sh(_earith.exp_2);
       render(")");
    }
    if (foo instanceof Ann.Absyn.EI2D)
    {
       Ann.Absyn.EI2D _ei2d = (Ann.Absyn.EI2D) foo;
       render("(");
       render("EI2D");
       sh(_ei2d.exp_);
       render(")");
    }
  }

  private static void sh(Ann.Absyn.Type foo)
  {
    if (foo instanceof Ann.Absyn.TInt)
    {
       Ann.Absyn.TInt _tint = (Ann.Absyn.TInt) foo;
       render("TInt");
    }
    if (foo instanceof Ann.Absyn.TDouble)
    {
       Ann.Absyn.TDouble _tdouble = (Ann.Absyn.TDouble) foo;
       render("TDouble");
    }
  }

  private static void sh(Ann.Absyn.Op foo)
  {
    if (foo instanceof Ann.Absyn.Times)
    {
       Ann.Absyn.Times _times = (Ann.Absyn.Times) foo;
       render("Times");
    }
    if (foo instanceof Ann.Absyn.Div)
    {
       Ann.Absyn.Div _div = (Ann.Absyn.Div) foo;
       render("Div");
    }
    if (foo instanceof Ann.Absyn.Plus)
    {
       Ann.Absyn.Plus _plus = (Ann.Absyn.Plus) foo;
       render("Plus");
    }
    if (foo instanceof Ann.Absyn.Minus)
    {
       Ann.Absyn.Minus _minus = (Ann.Absyn.Minus) foo;
       render("Minus");
    }
  }


  private static void pp(Integer n, int _i_) { buf_.append(n); buf_.append(" "); }
  private static void pp(Double d, int _i_) { buf_.append(d); buf_.append(" "); }
  private static void pp(String s, int _i_) { buf_.append(s); buf_.append(" "); }
  private static void pp(Character c, int _i_) { buf_.append("'" + c.toString() + "'"); buf_.append(" "); }
  private static void sh(Integer n) { render(n.toString()); }
  private static void sh(Double d) { render(d.toString()); }
  private static void sh(Character c) { render(c.toString()); }
  private static void sh(String s) { printQuoted(s); }
  private static void printQuoted(String s) { render("\"" + s + "\""); }
  private static void indent()
  {
    int n = _n_;
    while (n > 0)
    {
      buf_.append(" ");
      n--;
    }
  }
  private static void backup()
  {
     if (buf_.charAt(buf_.length() - 1) == ' ') {
      buf_.setLength(buf_.length() - 1);
    }
  }
  private static void trim()
  {
     while (buf_.length() > 0 && buf_.charAt(0) == ' ')
        buf_.deleteCharAt(0);
    while (buf_.length() > 0 && buf_.charAt(buf_.length()-1) == ' ')
        buf_.deleteCharAt(buf_.length()-1);
  }
  private static int _n_ = 0;
  private static StringBuilder buf_ = new StringBuilder(INITIAL_BUFFER_SIZE);
}

