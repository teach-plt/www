
  void compile(Ann.Stm s) {
    switch(s) {
      case Ann.SDecl p -> { newVar(p.name(), p.type()); }
      case Ann.SBlock p -> { newBlock(); compile(p.stms()); popBlock(); }
      case Ann.SExp p -> { compile(p.exp()); emit(new Code.Pop(p.exp().type())); }
      case Ann.SReturn p -> { compile(p.exp()); emit(new Code.Return(p.exp().type())); }

      default -> throw new RuntimeException("compile(Stm): case nyi");
    }
  }

  void compile(Ann.Exp e) {
    switch(e) {
      case Ann.EInt p -> { emit (new Code.IConst(p.lit()));}
      case Ann.EAss p -> {
        int addr = lookupVar(p.name()).addr();
        compile(p.exp());
        emit (new Code.Store(p.type(), addr));
        emit (new Code.Load(p.type(), addr));
      }
      case Ann.EId p -> {
        int addr = lookupVar(p.name()).addr();
        emit (new Code.Load(p.type(), addr));
      }
      case Ann.EApp p -> {
        for (Ann.Exp e1 : p.args()) compile(e1);
        Fun f = lookupFun(p.fun());
        emit(new Code.Call(f));
      }
      default -> throw new RuntimeException("compile(Exp): case nyi");
    }
  }
