module Compiler where

import Control.Monad
import Control.Monad.State
import System.Environment (getArgs)
import System.Exit (exitFailure)

import AbsMini
import LexMini
import ParMini
import PrintMini
import ErrM

-- a simple-minded compiler that doesn't need type annotations and works for integers only

compile :: String -> Program -> String
compile name p = unlines $ reverse $ code $ execState (compileProgram name p) emptyEnv

compileProgram :: String -> Program -> State Env ()
compileProgram name (Prog stms) = do
  mapM_ emit [
    ".class public " ++ name,
    ".super java/lang/Object",
    "",
    ".method public <init>()V",
    "aload_0",
    "invokenonvirtual java/lang/Object/<init>()V",
    "return",
    ".end method",
    "",
    ".method public static main([Ljava/lang/String;)V",
    ".limit locals 100",  --- bogus limit
    ".limit stack 1000"   --- bogus limit
   ]
  mapM_ compileStm stms
  emit "return"
  emit ".end method"

compileStm :: Stm -> State Env ()
compileStm s = case s of
  SDecl t x   -> addVar x t
  SAss x e -> do
    compileExp e
    a <- lookupVar x
    emit ("istore " ++ show a) 
  SBlock stms -> do
    a <- newBlock
    mapM compileStm stms
    exitBlock a
  SPrint e -> do
    compileExp e
    emit $ "invokestatic Runtime/printInt(I)V"

compileExp :: Exp -> State Env ()
compileExp e = case e of
  EVar x  -> do
    a <- lookupVar x
    emit ("iload " ++ show a)
  EInt i    -> emit ("bipush " ++ show i)
  EDouble d -> emit ("ldc2_w " ++ show d)
  EAdd e1 e2 -> do
    compileExp e1
    compileExp e2
    emit "iadd"
  ETyped _ e -> compileExp e

data Env = E {
  addresses   :: [[(Ident,Address)]],
  nextLabel   :: Int,
  nextAddress :: Address,
  maxAddress  :: Address,
  stackSize   :: Int,
  maxSize     :: Int,
  code        :: [Instruction]
  }

emptyEnv :: Env
emptyEnv = E {
  addresses = [[]],
  nextLabel = 0,
  nextAddress = 1,
  maxAddress = 1,
  stackSize = 0,
  maxSize = 1,
  code = []
  }

type Instruction = String
type Address = Int

emit :: Instruction -> State Env ()
emit i = modify (\env -> env{code = i : code env})

addVar :: Ident -> Type -> State Env ()
addVar x t = modify (\env -> env {
  addresses = case addresses env of (scope:rest) -> (((x,nextAddress env):scope):rest),
  nextAddress = nextAddress env + typeSize t
  })

typeSize :: Type -> Int
typeSize t = case t of
  TInt -> 1
  TDouble -> 2

lookupVar :: Ident -> State Env Address
lookupVar x = do
  env <- get
  return $ look (addresses env) x 
 where
   look [] x = error $ "Unknown variable " ++ printTree x ++ "."
   look (scope:rest) x = case lookup x scope of
     Nothing -> look rest x
     Just a  -> a

newBlock :: State Env Address
newBlock = do
  modify (\env -> env {addresses = [] : addresses env})
  env <- get
  return $ nextAddress env

exitBlock :: Address -> State Env ()
exitBlock a = modify (\env -> env {
   addresses = tail (addresses env),
   nextAddress = a
   })
