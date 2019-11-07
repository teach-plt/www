package Code;
import Code.Absyn.*;

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
  public static String print(Code.Absyn.Ins foo)
  {
    pp(foo, 0);
    trim();
    String temp = buf_.toString();
    buf_.delete(0,buf_.length());
    return temp;
  }
  public static String show(Code.Absyn.Ins foo)
  {
    sh(foo);
    String temp = buf_.toString();
    buf_.delete(0,buf_.length());
    return temp;
  }
  /***   You shouldn't need to change anything beyond this point.   ***/

  private static void pp(Code.Absyn.Ins foo, int _i_)
  {
    if (foo instanceof Code.Absyn.ILoad)
    {
       Code.Absyn.ILoad _iload = (Code.Absyn.ILoad) foo;
       if (_i_ > 0) render(_L_PAREN);
       render("iload");
       pp(_iload.integer_, 0);
       if (_i_ > 0) render(_R_PAREN);
    }
    else     if (foo instanceof Code.Absyn.IStore)
    {
       Code.Absyn.IStore _istore = (Code.Absyn.IStore) foo;
       if (_i_ > 0) render(_L_PAREN);
       render("istore");
       pp(_istore.integer_, 0);
       if (_i_ > 0) render(_R_PAREN);
    }
    else     if (foo instanceof Code.Absyn.IAdd)
    {
       Code.Absyn.IAdd _iadd = (Code.Absyn.IAdd) foo;
       if (_i_ > 0) render(_L_PAREN);
       render("iadd");
       if (_i_ > 0) render(_R_PAREN);
    }
    else     if (foo instanceof Code.Absyn.ISub)
    {
       Code.Absyn.ISub _isub = (Code.Absyn.ISub) foo;
       if (_i_ > 0) render(_L_PAREN);
       render("isub");
       if (_i_ > 0) render(_R_PAREN);
    }
    else     if (foo instanceof Code.Absyn.IMul)
    {
       Code.Absyn.IMul _imul = (Code.Absyn.IMul) foo;
       if (_i_ > 0) render(_L_PAREN);
       render("imul");
       if (_i_ > 0) render(_R_PAREN);
    }
    else     if (foo instanceof Code.Absyn.IDiv)
    {
       Code.Absyn.IDiv _idiv = (Code.Absyn.IDiv) foo;
       if (_i_ > 0) render(_L_PAREN);
       render("idiv");
       if (_i_ > 0) render(_R_PAREN);
    }
    else     if (foo instanceof Code.Absyn.ILit)
    {
       Code.Absyn.ILit _ilit = (Code.Absyn.ILit) foo;
       if (_i_ > 0) render(_L_PAREN);
       render("ldc");
       pp(_ilit.integer_, 0);
       if (_i_ > 0) render(_R_PAREN);
    }
    else     if (foo instanceof Code.Absyn.DLoad)
    {
       Code.Absyn.DLoad _dload = (Code.Absyn.DLoad) foo;
       if (_i_ > 0) render(_L_PAREN);
       render("dload");
       pp(_dload.integer_, 0);
       if (_i_ > 0) render(_R_PAREN);
    }
    else     if (foo instanceof Code.Absyn.DStore)
    {
       Code.Absyn.DStore _dstore = (Code.Absyn.DStore) foo;
       if (_i_ > 0) render(_L_PAREN);
       render("dstore");
       pp(_dstore.integer_, 0);
       if (_i_ > 0) render(_R_PAREN);
    }
    else     if (foo instanceof Code.Absyn.DAdd)
    {
       Code.Absyn.DAdd _dadd = (Code.Absyn.DAdd) foo;
       if (_i_ > 0) render(_L_PAREN);
       render("dadd");
       if (_i_ > 0) render(_R_PAREN);
    }
    else     if (foo instanceof Code.Absyn.DSub)
    {
       Code.Absyn.DSub _dsub = (Code.Absyn.DSub) foo;
       if (_i_ > 0) render(_L_PAREN);
       render("dsub");
       if (_i_ > 0) render(_R_PAREN);
    }
    else     if (foo instanceof Code.Absyn.DMul)
    {
       Code.Absyn.DMul _dmul = (Code.Absyn.DMul) foo;
       if (_i_ > 0) render(_L_PAREN);
       render("dmul");
       if (_i_ > 0) render(_R_PAREN);
    }
    else     if (foo instanceof Code.Absyn.DDiv)
    {
       Code.Absyn.DDiv _ddiv = (Code.Absyn.DDiv) foo;
       if (_i_ > 0) render(_L_PAREN);
       render("ddiv");
       if (_i_ > 0) render(_R_PAREN);
    }
    else     if (foo instanceof Code.Absyn.DLit)
    {
       Code.Absyn.DLit _dlit = (Code.Absyn.DLit) foo;
       if (_i_ > 0) render(_L_PAREN);
       render("ldc2_w");
       pp(_dlit.double_, 0);
       if (_i_ > 0) render(_R_PAREN);
    }
    else     if (foo instanceof Code.Absyn.I2D)
    {
       Code.Absyn.I2D _i2d = (Code.Absyn.I2D) foo;
       if (_i_ > 0) render(_L_PAREN);
       render("i2d");
       if (_i_ > 0) render(_R_PAREN);
    }
    else     if (foo instanceof Code.Absyn.IPrint)
    {
       Code.Absyn.IPrint _iprint = (Code.Absyn.IPrint) foo;
       if (_i_ > 0) render(_L_PAREN);
       render("invokestatic");
       render("Runtime/print(I)V");
       if (_i_ > 0) render(_R_PAREN);
    }
    else     if (foo instanceof Code.Absyn.DPrint)
    {
       Code.Absyn.DPrint _dprint = (Code.Absyn.DPrint) foo;
       if (_i_ > 0) render(_L_PAREN);
       render("invokestatic");
       render("Runtime/print(D)V");
       if (_i_ > 0) render(_R_PAREN);
    }
  }


  private static void sh(Code.Absyn.Ins foo)
  {
    if (foo instanceof Code.Absyn.ILoad)
    {
       Code.Absyn.ILoad _iload = (Code.Absyn.ILoad) foo;
       render("(");
       render("ILoad");
       sh(_iload.integer_);
       render(")");
    }
    if (foo instanceof Code.Absyn.IStore)
    {
       Code.Absyn.IStore _istore = (Code.Absyn.IStore) foo;
       render("(");
       render("IStore");
       sh(_istore.integer_);
       render(")");
    }
    if (foo instanceof Code.Absyn.IAdd)
    {
       Code.Absyn.IAdd _iadd = (Code.Absyn.IAdd) foo;
       render("IAdd");
    }
    if (foo instanceof Code.Absyn.ISub)
    {
       Code.Absyn.ISub _isub = (Code.Absyn.ISub) foo;
       render("ISub");
    }
    if (foo instanceof Code.Absyn.IMul)
    {
       Code.Absyn.IMul _imul = (Code.Absyn.IMul) foo;
       render("IMul");
    }
    if (foo instanceof Code.Absyn.IDiv)
    {
       Code.Absyn.IDiv _idiv = (Code.Absyn.IDiv) foo;
       render("IDiv");
    }
    if (foo instanceof Code.Absyn.ILit)
    {
       Code.Absyn.ILit _ilit = (Code.Absyn.ILit) foo;
       render("(");
       render("ILit");
       sh(_ilit.integer_);
       render(")");
    }
    if (foo instanceof Code.Absyn.DLoad)
    {
       Code.Absyn.DLoad _dload = (Code.Absyn.DLoad) foo;
       render("(");
       render("DLoad");
       sh(_dload.integer_);
       render(")");
    }
    if (foo instanceof Code.Absyn.DStore)
    {
       Code.Absyn.DStore _dstore = (Code.Absyn.DStore) foo;
       render("(");
       render("DStore");
       sh(_dstore.integer_);
       render(")");
    }
    if (foo instanceof Code.Absyn.DAdd)
    {
       Code.Absyn.DAdd _dadd = (Code.Absyn.DAdd) foo;
       render("DAdd");
    }
    if (foo instanceof Code.Absyn.DSub)
    {
       Code.Absyn.DSub _dsub = (Code.Absyn.DSub) foo;
       render("DSub");
    }
    if (foo instanceof Code.Absyn.DMul)
    {
       Code.Absyn.DMul _dmul = (Code.Absyn.DMul) foo;
       render("DMul");
    }
    if (foo instanceof Code.Absyn.DDiv)
    {
       Code.Absyn.DDiv _ddiv = (Code.Absyn.DDiv) foo;
       render("DDiv");
    }
    if (foo instanceof Code.Absyn.DLit)
    {
       Code.Absyn.DLit _dlit = (Code.Absyn.DLit) foo;
       render("(");
       render("DLit");
       sh(_dlit.double_);
       render(")");
    }
    if (foo instanceof Code.Absyn.I2D)
    {
       Code.Absyn.I2D _i2d = (Code.Absyn.I2D) foo;
       render("I2D");
    }
    if (foo instanceof Code.Absyn.IPrint)
    {
       Code.Absyn.IPrint _iprint = (Code.Absyn.IPrint) foo;
       render("IPrint");
    }
    if (foo instanceof Code.Absyn.DPrint)
    {
       Code.Absyn.DPrint _dprint = (Code.Absyn.DPrint) foo;
       render("DPrint");
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

