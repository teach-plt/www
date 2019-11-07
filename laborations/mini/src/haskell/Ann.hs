-- | Type-annotated internal syntax of MiniJS with coercions.

module Ann where

import MiniJS.Abs (Ident)
import Types

data Program = Prg [Stm]

data Stm
  = SAssign Type Ident Exp
  | SPrint  Type Exp

data Exp
  = EVar    Type Ident
  | EInt    Integer
  | EDouble Double
  | EArith  ArithOp Type Exp Exp
  | EI2D    Exp                   -- ^ Coerce from Int to Double

data ArithOp = Times | Div | Plus | Minus
