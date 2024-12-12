{-# OPTIONS_GHC -Wno-unused-imports #-} -- Turn off unused import warning off in stub
{-# OPTIONS_GHC -Wno-unused-matches #-} -- Turn off unused binding warning off in stub

{-# LANGUAGE LambdaCase #-}

-- | Interpreter for lambda-calculus with if, +, -, <.
--
--   Strategy can be either call-by-value or call-by-name.

module Interpreter (interpret, Strategy(..)) where

import Control.Applicative
import Control.Monad.Except
import Control.Monad.Reader

import Data.Functor
import Data.Map (Map)
import qualified Data.Map as Map

import Fun.Abs
import Fun.Print

-- | Evaluation strategy.

data Strategy
  = CallByName
  | CallByValue

-- | Error monad.

type Err = Except String

-- | Signature maps identifiers to closed expressions.

type Sig = Map Ident Exp

-- | Call-by-value environment maps identifiers to values.

type Env = Map Ident Val

-- | Values.

data Val
  = VInt Integer        -- ^ Integer literal.
  | VFun Ident Exp Env  -- ^ Function closure.

-- | Read-only data for the evaluator.

data EvDat = EvDat
  { _strategy :: Strategy
  , _sig      :: Sig
  }

-- | Entry point: Program computes a number.

interpret :: Strategy -> Program -> Err Integer
interpret strategy (Prog defs (DMain mainExp)) = do
  -- Build Sig
  let sig = Map.fromList $ map (\ (DDef f xs e) -> (f, abstract xs e)) defs
  -- Evaluate main
  case runExcept (eval (EvDat strategy sig) Map.empty mainExp) of
    Left err -> throwError err
    Right v -> case v of
      VInt i -> return i
      VFun{} -> throwError $ "main returned a function, but should return an integer"

-- | @abstract [x1,...,xn] e = EAbs x1 $ ... $ EAbs xn e@.
abstract :: [Ident] -> Exp -> Exp
abstract xs e = foldr EAbs e xs

eval :: EvDat -> Env -> Exp -> Err Val
eval dat env = \case
  EInt i     -> return $ VInt i  -- return :: Val -> Err Val
  EVar x     -> lookupVar dat env x
  EAbs x e   -> return $ VFun x e env
  EApp e1 e2 -> do
    v1 <- eval dat env e1
    v2 <- eval dat env e2
    case v1 of
      VFun x e env1 -> eval dat (Map.insert x v2 env1) e
      VInt{} -> throwError $ printTree e1 ++ " is not a function, but applied to " ++ printTree e2
  e -> nyi e

lookupVar :: EvDat -> Env -> Ident -> Err Val
lookupVar dat env x =
  case Map.lookup x env of
    Just v  -> return v
    Nothing ->
      case Map.lookup x (_sig dat) of
        Just e  -> eval dat Map.empty e
        Nothing -> throwError $ "unbound identifier " ++ printTree x

nyi :: Print e => e -> Err a
nyi e = throwError $ "not yet implemented: interpreting " ++ printTree e
