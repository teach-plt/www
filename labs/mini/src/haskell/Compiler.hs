{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}

-- | Compile MiniJS to JVM.
--
--   * Resolve variables to addresses.
--   * Resolve expression trees to stack instructions.

module Compiler where

import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Writer

import Data.Map (Map)
import qualified Data.Map as Map

import Ann
import JVM
import Types

-- | Compilation state.

data St = St
  { stNextAddr :: Addr
    -- ^ Next available address.
  , stCxt      :: Map Var Addr
    -- ^ Compilation environment, maps variables to addresses.
  }

-- | Compilation monad.

type Compile = StateT St (Writer [Ins])

-- | Compile a program to JVM instructions.

compile :: Program -> [Ins]
compile (Prg ss) = execWriter $ compileStms ss `runStateT` St 0 Map.empty

-- | Compile and return the type of an expression.

compileExp :: Exp -> Compile ()
compileExp = \case

  EVar t x     -> do
    addr <- lookupVar x
    emit $ load_ t addr

  EInt    i    -> emit $ ILit i
  EDouble d    -> emit $ DLit d

  EI2D   e     -> do
    compileExp e
    emit $ I2D

  EArith op t e1 e2 -> do
    compileExp e1
    compileExp e2
    emit $ case op of
      Times -> mul_ t
      Div   -> div_ t
      Plus  -> add_ t
      Minus -> sub_ t

-- | Compile a statement.

compileStm :: Stm -> Compile ()
compileStm = \case

  SAssign t x e -> do
    compileExp e
    addr <- lookupOrNewVar t x
    emit $ store_ t addr

  SPrint t e -> do
    compileExp e
    emit $ print_ t

-- | Compile a list of statements.

compileStms :: [Stm] -> Compile ()
compileStms = mapM_ compileStm

-- * Services of the Compile monad.

-- | Output an instruction.

emit :: Ins -> Compile ()
emit ins = tell [ins]

-- | Lookup the address of a variable.

lookupVar :: Var -> Compile Addr
lookupVar x = do
  cxt <- gets stCxt
  return $ cxt Map.! x

-- | Lookup the address of a variable, or create a new one for a new variable.

lookupOrNewVar :: Type -> Var -> Compile Addr
lookupOrNewVar t x = do
  cxt <- gets stCxt
  case Map.lookup x cxt of
    Just addr -> return addr
    Nothing   -> do
      addr <- gets stNextAddr
      put $ St (addr + sizeof t) (Map.insert x addr cxt)
      return addr
