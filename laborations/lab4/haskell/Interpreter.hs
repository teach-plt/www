-- | Interpreter for lambda-calculus with if, +, -, <.
--
--   Strategy can be either call-by-value or call-by-name.

module Interpreter where

import Control.Applicative
import Control.Monad.Reader
import Control.Monad.State

import Data.Functor
import Data.Map (Map)
import qualified Data.Map as Map

import Fun.Abs
import Fun.ErrM
import Fun.Print

-- | Evaluation strategy.

data Strategy
  = CallByName
  | CallByValue

-- | Entry point: Program computes a number.

interpret :: Strategy -> Program -> Err Integer
interpret strategy (Prog defs (DMain mainExp)) = do

  -- Create initial environment
  let cxt = Cxt strategy sig Map.empty
      sig = Map.fromList $ map mkSigEntry defs
      mkSigEntry :: Def -> (Ident, Exp)
      mkSigEntry (DDef f xs body) = (f, foldr EAbs body xs)

  -- Run the interpreter.
  v <- eval mainExp `runReaderT` cxt

  -- Inspect the result.
  case v of
    VInt i -> return i
    VFun{} -> fail $ "main returned a function, but should return an integer"

-- * Data structures for the interpreter.

-- | Values.

data Value
  = VInt Integer       -- ^ Numeric value.
  | VFun Ident Exp Env -- ^ Function value.

-- | Context.

data Cxt = Cxt
  { cxtStrategy :: Strategy  -- ^ Evaluation strategy (fixed).
  , cxtSig      :: Sig       -- ^ Binds function identifiers to expression.
  , cxtEnv      :: Env       -- ^ Binds local variables to values.
  }

-- | Signature.

type Sig = Map Ident Exp  -- ^ 'Exp' is closed

-- | Environment for call-by-value.

type Env = Map Ident Value

-- | Evaluation monad.

type Eval = ReaderT Cxt Err

-- * Interpreter.

todo = error "not yet implemented, TODO!"

-- | Evaluation.
eval :: Exp -> Eval Value
eval e = case e of

  EInt i    -> return $ VInt i

  EVar x    -> do
    -- Look in the local environment
    env <- asks cxtEnv
    case Map.lookup x env of
      Just v -> return v
      Nothing -> do
        -- Look in the global signature
        sig <- asks cxtSig
        case Map.lookup x sig of
          Just e  -> do
            -- Evaluate e in the empty local environment
            inEnv Map.empty $ eval e
          Nothing -> fail $ "Unbound identifier " ++ printTree x

  EAbs x e  -> do
    env <- asks cxtEnv
    return $ VFun x e env

  EApp f e  -> do
    vf <- eval f
    ve <- eval e
    case vf of
      VFun x f' env -> inEnv (Map.insert x ve env) $ eval f'
      VInt{}        -> fail $ "Applying non-function"

  EAdd e e' -> todo
  ESub e e' -> todo
  ELt  e e' -> todo
  EIf c t e -> todo







-- ** Managing the environment

-- | Perform an Eval action in a given environment.
inEnv :: Env -> Eval a -> Eval a
inEnv env computation = do
  local (\ cxt -> cxt { cxtEnv = env}) computation

-- ** Managing the signature

-- * Auxiliary definitions

-- -- | Printing values
-- instance Print Value where
--   prt _ (VInt i) = prt 0 i
--   prt _ _        = doc $ showString "<function>"

-- | Monadic application to two arguments
(==<<) :: Monad m => (a -> b -> m c) -> (m a, m b) -> m c
k ==<< (ma , mb) = do
  a <- ma
  b <- mb
  k a b
