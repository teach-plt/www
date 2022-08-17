{-# OPTIONS_GHC -Wno-unused-imports #-} -- Turn off unused import warning off in stub

-- | Compiler for C--, producing symbolic JVM assembler.

module Compiler where

import Control.Monad
import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.RWS

import Data.Maybe
import Data.Map (Map)
import qualified Data.Map as Map

import Annotated

-- | Entry point.

compile
  :: String  -- ^ Class name.
  -> Program -- ^ Type-annotated program.
  -> String  -- ^ Generated jasmin source file content.
compile name _prg = header
  where
  header :: String
  header = unlines $ concat
    [ [ ";; BEGIN HEADER"
      , ""
      , ".class public " ++ name
      , ".super java/lang/Object"
      , ""
      , ".method public <init>()V"
      , ".limit locals 1"
      , ""
      ]
    , map indent
      [ "aload_0"
      , "invokespecial java/lang/Object/<init>()V"
      , "return"
      ]
    , [ ""
      , ".end method"
      , ""
      , ".method public static main([Ljava/lang/String;)V"
      , ".limit locals 1"
      , ".limit stack  1"
      , ""
      ]
    , map indent
      [ "invokestatic " ++ name ++ "/main()I"
      , "pop"
      , "return"
      ]
    , [ ""
      , ".end method"
      , ""
      , ";; END HEADER"
      ]
    ]

-- | Indent non-empty lines.

indent :: String -> String
indent s = if null s then s else "\t" ++ s
