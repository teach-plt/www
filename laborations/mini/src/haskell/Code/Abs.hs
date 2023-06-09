-- Haskell data types for the abstract syntax.
-- Generated by the BNF converter.

module Code.Abs where

data Ins
    = ILoad Integer
    | IStore Integer
    | IAdd
    | ISub
    | IMul
    | IDiv
    | ILit Integer
    | DLoad Integer
    | DStore Integer
    | DAdd
    | DSub
    | DMul
    | DDiv
    | DLit Double
    | I2D
    | IPrint
    | DPrint
  deriving (Eq, Ord, Show, Read)

