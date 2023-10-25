module AnnotatingTypeChecker where

import AbsMini
import PrintMini
import ErrM


-- from TypeChecker by changing () to Program,Stm,etc
typecheck :: Program -> Err Program
typecheck (Prog stms) = do (stms',_) <- checkStms emptyEnv stms ; return (Prog stms')

checkStms :: Env -> [Stm] -> Err ([Stm],Env)
checkStms env [] = return ([],env)
checkStms env (st:stms) = do   
  (st',env')    <- checkStm env st   -- a bit more like the interpreter!
  (stms',env'') <- checkStms env' stms
  return (st':stms', env'')

checkStm :: Env -> Stm -> Err (Stm,Env)
checkStm env s = 
    case s of
      SDecl t x       -> do env' <- addVar env x t ; return (s,env')
      SAss x e        -> do t <- lookupVar env x
                            e' <- checkExp env e t
                            return (SAss x e', env)      
      SBlock stms     -> do (stms',_) <- checkStms (addScope env) stms
                            return (SBlock stms',env)  -- discard env updates
      SPrint e        -> do (e',_) <- inferExp env e
                            return (SPrint e', env)

checkExp :: Env -> Exp -> Type -> Err Exp
checkExp env e t = 
    do (e',t') <- inferExp env e
       if t' /= t 
         then fail (printTree e ++ " has type " ++ printTree t'
                    ++ " expected " ++ printTree t)
         else return e'

inferExp :: Env -> Exp -> Err (Exp,Type)
inferExp env e = 
    case e of
      EVar x         -> do t <- lookupVar env x ; return (ETyped t e, t)
      EInt _         -> return (ETyped TInt e,TInt)
      EDouble _      -> return (ETyped TDouble e, TDouble)
      EAdd e1 e2     -> do (e1',t1) <- inferExp env e1
                           (e2',t2) <- inferExp env e2
                           if t1 == t2 
                             then return (ETyped t1 (EAdd e1' e2'), t1)
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
