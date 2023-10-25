{-# LANGUAGE LambdaCase #-}

-- | Annotating type checker.
--
--   Generates a syntax tree with type information.

module AnnTypeChecker where

import qualified Data.List as List
import Data.Map (Map)
import qualified Data.Map as Map

import MiniJS.Abs
import Types
import qualified Ann as A

-- | Typing environments (contexts) map variables to types

type Cxt = Map Var Type

-- | Type check a program.
--   Return the type-annotated version of the program.

typeCheck :: Program -> A.Program
typeCheck (Prg ss) = A.Prg ss'
  where
  (_, ss') = checks Map.empty ss

-- | Compute (infer) the type of an expression.

infer :: Cxt -> Exp -> (Type, A.Exp)
infer cxt = \case
  EVar    x    -> (t, A.EVar t x) where t = cxt Map.! x
  EInt    i    -> (TInt   , A.EInt    i)
  EDouble d    -> (TDouble, A.EDouble d)

  ETimes e1 e2 -> inferArith cxt (A.EArith A.Times) e1 e2
  EDiv   e1 e2 -> inferArith cxt (A.EArith A.Div  ) e1 e2
  EPlus  e1 e2 -> inferArith cxt (A.EArith A.Plus ) e1 e2
  EMinus e1 e2 -> inferArith cxt (A.EArith A.Minus) e1 e2

-- | Infer the type of an arithmetic expression.
--   Insert coercions from Int to Double if necessary..

inferArith
  :: Cxt
  -> (Type -> A.Exp -> A.Exp -> A.Exp)
  -> Exp
  -> Exp
  -> (Type, A.Exp)
inferArith cxt op e1 e2 = (t, op t (cast t1 t e1') (cast t2 t e2'))
  where
  (t1, e1') = infer cxt e1
  (t2, e2') = infer cxt e2
  t         = max t1 t2

-- | Insert a coercion if necessary.
cast
  :: Type   -- ^ Type of the expression.
  -> Type   -- ^ Supertype (or same type).
  -> A.Exp  -- ^ Original expression.
  -> A.Exp  -- ^ A coercion has been added if strict supertype.

cast t1 t2 e = if t1 < t2 then A.EI2D e else e

-- | Check that a statement is type-correct.
--
--   Variables cannot be assigned an expression of a different type
--   than the first time they were assigned.

check :: Cxt -> Stm -> (Cxt, A.Stm)
check cxt = \case

  SAssign x e -> (insertVar x t cxt, A.SAssign t x e')
    where (t, e') = infer cxt e

  SPrint e    -> (cxt, A.SPrint t e')
    where (t, e') = infer cxt e

-- | Check and annotate a sequence of statements.

checks :: Cxt -> [Stm] -> (Cxt, [A.Stm])
checks = List.mapAccumL check

-- * Operation on the context

-- | Insert a new binding into the context.
--   If the variable is already bound, ensure that it has the same type.

insertVar :: Var -> Type -> Cxt -> Cxt
insertVar x t cxt =
  case Map.lookup x cxt of
    Nothing -> Map.insert x t cxt
    Just t' -> if t == t' then cxt else error $ "invalid assignment"
