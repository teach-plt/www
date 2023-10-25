module TypeChecker where

import AbsMini
import PrintMini
import ErrM



typecheck :: Program -> Err ()
typecheck (Prog stms) = checkStms emptyEnv stms

checkStms :: Env -> [Stm] -> Err ()
checkStms env [] = return ()
checkStms env (st:stms) = do env' <- checkStm env st
                             checkStms env' stms

checkStm :: Env -> Stm -> Err Env
checkStm env s = 
    case s of
      SDecl t x       -> addVar env x t
      SAss x e        -> do t <- lookupVar env x
                            checkExp env e t
                            return env      
      SBlock stms     -> do checkStms (addScope env) stms
                            return env
      SPrint e        -> do inferExp env e
                            return env

checkExp :: Env -> Exp -> Type -> Err ()
checkExp env e t = 
    do t' <- inferExp env e
       if t' /= t 
         then fail (printTree e ++ " has type " ++ printTree t'
                    ++ " expected " ++ printTree t)
         else return ()

inferExp :: Env -> Exp -> Err Type
inferExp env e = 
    case e of
      EVar x         -> lookupVar env x
      EInt _         -> return TInt
      EDouble _      -> return TDouble
      EAdd e1 e2     -> do t1 <- inferExp env e1
                           t2 <- inferExp env e2
                           if t1 == t2 
                             then return t1
                             else fail (printTree e1 ++ " has type " ++ printTree t1
                                         ++ " but " ++ printTree e2 
                                         ++ " has type " ++ printTree t2)

type Env = [[(Ident, Type)]]

emptyEnv :: Env
emptyEnv = [[]]

addVar :: Env -> Ident -> Type -> Err Env
addVar (scope:rest) x t = 
    case lookup x scope of
      Nothing -> return (((x,t):scope):rest)
      Just _  -> fail ("Variable " ++ printTree x ++ " already declared.")

lookupVar :: Env -> Ident -> Err Type
lookupVar [] x = fail $ "Unknown variable " ++ printTree x ++ "."
lookupVar (scope:rest) x = case lookup x scope of
                             Nothing -> lookupVar rest x
                             Just t  -> return t
addScope :: Env -> Env
addScope env = []:env
