-- | Higher-level access to JVM syntax.

module JVM
  ( module JVM
  , Ins(ILit, DLit, I2D)
  ) where

import Code.Abs
import Code.Print
import Types

type Addr = Integer

-- | Type-parameterized version of load and store.

load_, store_ :: Type -> Addr -> Ins
load_ TInt     = ILoad
load_ TDouble  = DLoad
store_ TInt    = IStore
store_ TDouble = DStore

-- | Type-parameterized version of arithmetic instructions.

mul_, div_, add_, sub_ :: Type -> Ins
mul_ TInt    = IMul
mul_ TDouble = DMul
div_ TInt    = IDiv
div_ TDouble = DDiv
add_ TInt    = IAdd
add_ TDouble = DAdd
sub_ TInt    = ISub
sub_ TDouble = DSub

-- | Type-parameterized version of print "instruction".

print_ :: Type -> Ins
print_ TInt    = IPrint
print_ TDouble = DPrint

-- | Size of a value of the given type in 32bit units.

sizeof :: Type -> Integer
sizeof TInt    = 1
sizeof TDouble = 2

-- | Generate Jasmin file content.

mainClass
  :: String  -- ^ Name of the class.
  -> [Ins]   -- ^ Instruction list for the main method.
  -> String  -- ^ Jasmin text.

mainClass name ins = unlines $ concat
  [ [ ".class public " ++ name
    , ".super java/lang/Object"
    , ""
    , ".method public <init>()V"
    , "  .limit locals 1"
    , ""
    , "  aload_0"
    , "  invokespecial java/lang/Object/<init>()V"
    , "  return"
    , ""
    , ".end method"
    , ""
    , ".method public static main([Ljava/lang/String;)V"
    , "  .limit locals 1000"  -- dummy
    , "  .limit stack  1000"  -- dummy
    , ""
    ]
  , map (("  " ++) . printTree) ins
  , [ "  return"
    , ""
    , ".end method"
    ]
  ]
