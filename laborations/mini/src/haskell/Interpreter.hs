{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE Rank2Types #-}

-- | Interpreter for MiniJS.

module Interpreter where

import Data.Map (Map)
import qualified Data.Map as Map

import MiniJS.Abs

-- | Values are either integer or double literals.

data Val
  = VInt    Integer
  | VDouble Double

-- | Environments map variables to their values.

type Var = Ident
type Env = Map Var Val

-- | Interpreting a program performs some output.

interpret :: Program -> IO ()
interpret (Prg ss) = do
  _ <- execs Map.empty ss
  return ()

-- | Evaluate an expression to a value.

eval :: Env -> Exp -> Val
eval env = \case
  EVar x       -> env Map.! x
  EInt i       -> VInt i
  EDouble d    -> VDouble d
  ETimes e1 e2 -> arith (*) (eval env e1) (eval env e2)
  EDiv   e1 e2 -> divide    (eval env e1) (eval env e2)
  EPlus  e1 e2 -> arith (+) (eval env e1) (eval env e2)
  EMinus e1 e2 -> arith (-) (eval env e1) (eval env e2)

-- | Execute a statement: may change the environment and produce some output.

exec :: Env -> Stm -> IO Env
exec env = \case

  SAssign x e -> do
    let v = eval env e
    return $ Map.insert x v env

  SPrint e    -> do
    printVal $ eval env e
    return env

-- | Execute a sequence of statments.

execs :: Env -> [Stm] -> IO Env
execs env []     = return env
execs env (s:ss) = do
  env' <- exec env s
  execs env' ss

-- * Operations on values

-- | Perform an arithmetical operation on two values.

arith :: (forall a. Num a => a -> a -> a) -> Val -> Val -> Val
arith op (VInt    i1) (VInt    i2) = VInt    (op i1 i2)
arith op (VDouble d1) (VDouble d2) = VDouble (op d1 d2)
arith op (VInt    i1) (VDouble d2) = VDouble (op (fromIntegral i1) d2)
arith op (VDouble d1) (VInt    i2) = VDouble (op d1 (fromIntegral i2))

divide :: Val -> Val -> Val
divide (VInt    i1) (VInt    i2) = VInt    (i1 `div` i2)
divide (VDouble d1) (VDouble d2) = VDouble (d1 / d2)
divide (VInt    i1) (VDouble d2) = VDouble (fromIntegral i1 / d2)
divide (VDouble d1) (VInt    i2) = VDouble (d1 / fromIntegral i2)

-- | Print a value on stdout.

printVal :: Val -> IO ()
printVal (VInt    i) = print i
printVal (VDouble d) = print d
