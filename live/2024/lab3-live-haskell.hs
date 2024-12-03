
-- | Compile a function, to be called in initial environment.

compileFun :: Type -> [Arg] -> [Stm] -> Compile ()
compileFun t args ss = do
    mapM_ (\ (ADecl t' x) -> newVar x t') args
    mapM_ compileStm ss
    -- Default return
    -- Push 0 on the stack, depending on t
    if t == Type_double then emit $ DConst 0.0 else
      if t /= Type_void then emit $ IConst 0
        else pure ()
    emit $ Return t
-- | Compile a statement.

compileStm :: Stm -> Compile ()
compileStm s0 = do

  -- Output a comment with the statement to compile.
  -- TODO

  -- Compile the statement.
  case s0 of

    SDecl t x -> newVar x t
    SExp t e -> do
      compileExp e
      emit $ Pop t
    SReturn t e -> do
      compileExp e
      emit $ Return t
    SBlock b -> compileBlock b
    s -> error $ "Not yet implemented: compileStm " ++ printTree s

-- | Compile a block.

compileBlock :: Block -> Compile ()
compileBlock (Block ss) = do
  inNewBlock $ mapM_ compileStm ss


-- | Compile an expression to leave its value on the stack.

compileExp :: Exp -> Compile ()
compileExp = \case
    EInt i -> emit $ IConst i
    EBool b -> emit $ IConst $ if b then 1 else 0
    EId x -> do
      (a, t) <- lookupVar x
      emit $ Load t a
    EAss x e -> do
      compileExp e
      (a, t) <- lookupVar x
      emit $ Store t a
      emit $ Load t a
    EApp t x es -> do
      mapM_ compileExp es
      m <- gets sig
      let f = Map.findWithDefault (error "undefined fun") x m
      emit $ Call f

    e -> error $ "Not yet implemented: compileExp " ++ printTree e
