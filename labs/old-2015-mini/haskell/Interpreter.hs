module Interpreter where

import Control.Monad
import System.Environment (getArgs)
import System.Exit (exitFailure)

import AbsMini
import LexMini
import ParMini
import PrintMini
import ErrM

data Value = VInt Integer | VDouble Double | VUndef

instance Show Value where
    show (VInt i)    = show i
    show (VDouble d) = show d
    show VUndef      = "undefined"

interpret :: Program -> IO ()
interpret (Prog stms) = do execStms emptyEnv stms
                           return ()

execStms :: Env -> [Stm] -> IO Env
execStms env [] = return env
execStms env (st:stms) = do env' <- execStm env st
                            execStms env' stms

execStm :: Env -> Stm -> IO Env
execStm env s = 
    case s of
      SDecl _ x       -> return (addVar env x)
      SAss x e        -> return (setVar env x (evalExp env e))
      SBlock stms     -> do env' <- execStms (enterScope env) stms
                            return (leaveScope env')
      SPrint e        -> do print (evalExp env e)
                            return env

evalExp :: Env -> Exp -> Value
evalExp env e = 
    case e of
      EVar x         -> lookupVar env x
      EInt i         -> VInt i
      EDouble d      -> VDouble d
      EAdd e1 e2     -> let v1 = evalExp env e1
                            v2 = evalExp env e2
                         in case (v1,v2) of
                              (VInt i1, VInt i2)       -> VInt (i1+i2)
                              (VDouble d1, VDouble d2) -> VDouble (d1+d2)

type Env = [[(Ident, Value)]]

emptyEnv :: Env
emptyEnv = [[]]

addVar :: Env -> Ident -> Env
addVar (scope:rest) x = (((x,VUndef):scope):rest)

setVar :: Env -> Ident -> Value -> Env
setVar [] x _ = error $ "Unknown variable " ++ printTree x ++ "."
setVar ([]:rest) x v = []:setVar rest x v
setVar ((p@(y,_):scope):rest) x v 
    | y == x = ((x,v):scope):rest
    | otherwise = let scope':rest' = setVar (scope:rest) x v
                   in (p:scope'):rest'

lookupVar :: Env -> Ident -> Value
lookupVar [] x = error $ "Unknown variable " ++ printTree x ++ "."
lookupVar (scope:rest) x = case lookup x scope of
                             Nothing -> lookupVar rest x
                             Just v  -> v
enterScope :: Env -> Env
enterScope env = []:env

leaveScope :: Env -> Env
leaveScope (_:env) = env
