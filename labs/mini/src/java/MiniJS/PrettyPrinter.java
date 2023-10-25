package MiniJS;
import MiniJS.Absyn.*;

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
  public static String print(MiniJS.Absyn.Program foo)
  {
    pp(foo, 0);
    trim();
    String temp = buf_.toString();
    buf_.delete(0,buf_.length());
    return temp;
  }
  public static String show(MiniJS.Absyn.Program foo)
  {
    sh(foo);
    String temp = buf_.toString();
    buf_.delete(0,buf_.length());
    return temp;
  }
  public static String print(MiniJS.Absyn.Stm foo)
  {
    pp(foo, 0);
    trim();
    String temp = buf_.toString();
    buf_.delete(0,buf_.length());
    return temp;
  }
  public static String show(MiniJS.Absyn.Stm foo)
  {
    sh(foo);
    String temp = buf_.toString();
    buf_.delete(0,buf_.length());
    return temp;
  }
  public static String print(MiniJS.Absyn.ListStm foo)
  {
    pp(foo, 0);
    trim();
    String temp = buf_.toString();
    buf_.delete(0,buf_.length());
    return temp;
  }
  public static String show(MiniJS.Absyn.ListStm foo)
  {
    sh(foo);
    String temp = buf_.toString();
    buf_.delete(0,buf_.length());
    return temp;
  }
  public static String print(MiniJS.Absyn.Exp foo)
  {
    pp(foo, 0);
    trim();
    String temp = buf_.toString();
    buf_.delete(0,buf_.length());
    return temp;
  }
  public static String show(MiniJS.Absyn.Exp foo)
  {
    sh(foo);
    String temp = buf_.toString();
    buf_.delete(0,buf_.length());
    return temp;
  }
  /***   You shouldn't need to change anything beyond this point.   ***/

  private static void pp(MiniJS.Absyn.Program foo, int _i_)
  {
    if (foo instanceof MiniJS.Absyn.Prg)
    {
       MiniJS.Absyn.Prg _prg = (MiniJS.Absyn.Prg) foo;
       if (_i_ > 0) render(_L_PAREN);
       pp(_prg.liststm_, 0);
       if (_i_ > 0) render(_R_PAREN);
    }
  }

  private static void pp(MiniJS.Absyn.Stm foo, int _i_)
  {
    if (foo instanceof MiniJS.Absyn.SAssign)
    {
       MiniJS.Absyn.SAssign _sassign = (MiniJS.Absyn.SAssign) foo;
       if (_i_ > 0) render(_L_PAREN);
       pp(_sassign.ident_, 0);
       render("=");
       pp(_sassign.exp_, 0);
       if (_i_ > 0) render(_R_PAREN);
    }
    else     if (foo instanceof MiniJS.Absyn.SPrint)
    {
       MiniJS.Absyn.SPrint _sprint = (MiniJS.Absyn.SPrint) foo;
       if (_i_ > 0) render(_L_PAREN);
       render("console.log");
       render("(");
       pp(_sprint.exp_, 0);
       render(")");
       if (_i_ > 0) render(_R_PAREN);
    }
  }

  private static void pp(MiniJS.Absyn.ListStm foo, int _i_)
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

  private static void pp(MiniJS.Absyn.Exp foo, int _i_)
  {
    if (foo instanceof MiniJS.Absyn.EVar)
    {
       MiniJS.Absyn.EVar _evar = (MiniJS.Absyn.EVar) foo;
       if (_i_ > 2) render(_L_PAREN);
       pp(_evar.ident_, 0);
       if (_i_ > 2) render(_R_PAREN);
    }
    else     if (foo instanceof MiniJS.Absyn.EInt)
    {
       MiniJS.Absyn.EInt _eint = (MiniJS.Absyn.EInt) foo;
       if (_i_ > 2) render(_L_PAREN);
       pp(_eint.integer_, 0);
       if (_i_ > 2) render(_R_PAREN);
    }
    else     if (foo instanceof MiniJS.Absyn.EDouble)
    {
       MiniJS.Absyn.EDouble _edouble = (MiniJS.Absyn.EDouble) foo;
       if (_i_ > 2) render(_L_PAREN);
       pp(_edouble.double_, 0);
       if (_i_ > 2) render(_R_PAREN);
    }
    else     if (foo instanceof MiniJS.Absyn.ETimes)
    {
       MiniJS.Absyn.ETimes _etimes = (MiniJS.Absyn.ETimes) foo;
       if (_i_ > 1) render(_L_PAREN);
       pp(_etimes.exp_1, 1);
       render("*");
       pp(_etimes.exp_2, 2);
       if (_i_ > 1) render(_R_PAREN);
    }
    else     if (foo instanceof MiniJS.Absyn.EDiv)
    {
       MiniJS.Absyn.EDiv _ediv = (MiniJS.Absyn.EDiv) foo;
       if (_i_ > 1) render(_L_PAREN);
       pp(_ediv.exp_1, 1);
       render("/");
       pp(_ediv.exp_2, 2);
       if (_i_ > 1) render(_R_PAREN);
    }
    else     if (foo instanceof MiniJS.Absyn.EPlus)
    {
       MiniJS.Absyn.EPlus _eplus = (MiniJS.Absyn.EPlus) foo;
       if (_i_ > 0) render(_L_PAREN);
       pp(_eplus.exp_1, 0);
       render("+");
       pp(_eplus.exp_2, 1);
       if (_i_ > 0) render(_R_PAREN);
    }
    else     if (foo instanceof MiniJS.Absyn.EMinus)
    {
       MiniJS.Absyn.EMinus _eminus = (MiniJS.Absyn.EMinus) foo;
       if (_i_ > 0) render(_L_PAREN);
       pp(_eminus.exp_1, 0);
       render("-");
       pp(_eminus.exp_2, 1);
       if (_i_ > 0) render(_R_PAREN);
    }
  }


  private static void sh(MiniJS.Absyn.Program foo)
  {
    if (foo instanceof MiniJS.Absyn.Prg)
    {
       MiniJS.Absyn.Prg _prg = (MiniJS.Absyn.Prg) foo;
       render("(");
       render("Prg");
       render("[");
       sh(_prg.liststm_);
       render("]");
       render(")");
    }
  }

  private static void sh(MiniJS.Absyn.Stm foo)
  {
    if (foo instanceof MiniJS.Absyn.SAssign)
    {
       MiniJS.Absyn.SAssign _sassign = (MiniJS.Absyn.SAssign) foo;
       render("(");
       render("SAssign");
       sh(_sassign.ident_);
       sh(_sassign.exp_);
       render(")");
    }
    if (foo instanceof MiniJS.Absyn.SPrint)
    {
       MiniJS.Absyn.SPrint _sprint = (MiniJS.Absyn.SPrint) foo;
       render("(");
       render("SPrint");
       sh(_sprint.exp_);
       render(")");
    }
  }

  private static void sh(MiniJS.Absyn.ListStm foo)
  {
     for (java.util.Iterator<Stm> it = foo.iterator(); it.hasNext();)
     {
       sh(it.next());
       if (it.hasNext())
         render(",");
     }
  }

  private static void sh(MiniJS.Absyn.Exp foo)
  {
    if (foo instanceof MiniJS.Absyn.EVar)
    {
       MiniJS.Absyn.EVar _evar = (MiniJS.Absyn.EVar) foo;
       render("(");
       render("EVar");
       sh(_evar.ident_);
       render(")");
    }
    if (foo instanceof MiniJS.Absyn.EInt)
    {
       MiniJS.Absyn.EInt _eint = (MiniJS.Absyn.EInt) foo;
       render("(");
       render("EInt");
       sh(_eint.integer_);
       render(")");
    }
    if (foo instanceof MiniJS.Absyn.EDouble)
    {
       MiniJS.Absyn.EDouble _edouble = (MiniJS.Absyn.EDouble) foo;
       render("(");
       render("EDouble");
       sh(_edouble.double_);
       render(")");
    }
    if (foo instanceof MiniJS.Absyn.ETimes)
    {
       MiniJS.Absyn.ETimes _etimes = (MiniJS.Absyn.ETimes) foo;
       render("(");
       render("ETimes");
       sh(_etimes.exp_1);
       sh(_etimes.exp_2);
       render(")");
    }
    if (foo instanceof MiniJS.Absyn.EDiv)
    {
       MiniJS.Absyn.EDiv _ediv = (MiniJS.Absyn.EDiv) foo;
       render("(");
       render("EDiv");
       sh(_ediv.exp_1);
       sh(_ediv.exp_2);
       render(")");
    }
    if (foo instanceof MiniJS.Absyn.EPlus)
    {
       MiniJS.Absyn.EPlus _eplus = (MiniJS.Absyn.EPlus) foo;
       render("(");
       render("EPlus");
       sh(_eplus.exp_1);
       sh(_eplus.exp_2);
       render(")");
    }
    if (foo instanceof MiniJS.Absyn.EMinus)
    {
       MiniJS.Absyn.EMinus _eminus = (MiniJS.Absyn.EMinus) foo;
       render("(");
       render("EMinus");
       sh(_eminus.exp_1);
       sh(_eminus.exp_2);
       render(")");
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

