-- | Types for MiniJS.

module Types where

import MiniJS.Abs (Ident)

data Type
  = TInt
  | TDouble
  deriving (Eq, Ord)

type Var = Ident
