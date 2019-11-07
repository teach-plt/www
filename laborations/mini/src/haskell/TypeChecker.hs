{-# LANGUAGE LambdaCase #-}

-- | Simple type checker for MiniJS.
--
--   Ensures that a variable is never used at different types.

module TypeChecker where

import Data.Map (Map)
import qualified Data.Map as Map

import MiniJS.Abs
import Types

-- | Typing environments (contexts) map variables to types.

type Cxt = Map Var Type

-- | Type check a program.
--   Returns the typing of the variables.

typeCheck :: Program -> Cxt
typeCheck (Prg ss) = checks Map.empty ss

-- | Compute (infer) the type of an expression.

infer :: Cxt -> Exp -> Type
infer cxt = \case
  EVar    x    -> cxt Map.! x
  EInt    i    -> TInt
  EDouble d    -> TDouble
  ETimes e1 e2 -> max (infer cxt e1) (infer cxt e2)
  EDiv   e1 e2 -> max (infer cxt e1) (infer cxt e2)
  EPlus  e1 e2 -> max (infer cxt e1) (infer cxt e2)
  EMinus e1 e2 -> max (infer cxt e1) (infer cxt e2)

-- | Check that a statement is type-correct.
--
--   Variables cannot be assigned an expression of a different type
--   than the first time they were assigned.

check :: Cxt -> Stm -> Cxt
check cxt = \case

  SAssign x e -> let t = infer cxt e in insertVar x t cxt
  SPrint e    -> infer cxt e `seq` cxt

-- | Check a sequence of statements.

checks :: Cxt -> [Stm] -> Cxt
checks = foldl check


-- * Operation on the context

-- | Insert a new binding into the context.
--   If the variable is already bound, ensure that it has the same type.

insertVar :: Var -> Type -> Cxt -> Cxt
insertVar x t cxt =
  case Map.lookup x cxt of
    Nothing -> Map.insert x t cxt
    Just t' -> if t == t' then cxt else error $ "invalid assignment"
