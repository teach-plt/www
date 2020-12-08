The interpreter in Haskell
==========================

We implement interpretation of expressions and statements via Haskell functions:

  - ``eval`` : evaluate an expression
  - ``exec`` : execute a statement
  - ``execs``: execute a statement sequence

The interpreter need some structure for the following tasks:

1. input/output (e.g. ``readInt``, ``printInt``),
2. read/write environment (of type ``Env``),
3. ``return`` a value (type ``Val``) from the middle of a computation, and
4. reading from a dictionary (the function _signature_ of type
   ``Sig``) that maps function names to their definition.

We can implement these structures as follows:

1. For input/output, there is the Haskell `IO` monad.

2. State threading: pass in the environment "before", return the
   environment "after".

3. Exception: the return value is an handled as exceptional value,
   propagated through the computation, caught at the function call.

4. Supply in the function signature as separate parameter; no need to
   return it, since it does not change.
   (Alternatively, we could make the function signature part of the
   environment and just never change it.)

The ``return`` exception can be thrown only when _statements_ are executed.


Explicit state passing and exception propagation
------------------------------------------------

The considerations above suggest the following type signatures
```haskell
eval  :: Exp   -> Sig -> Env -> IO (Val, Env)
exec  :: Stm   -> Sig -> Env -> IO (Either Val Env)
execs :: [Stm] -> Sig -> Env -> IO (Either Val Env)
```
and these implementations fragments:

- Binary operations
  ```haskell
  eval (EBinOp op e1 e2) sig env = do
    (v1, env1) <- eval e1 sig env
    (v2, env2) <- eval e2 sig env1
    v <- binOp op v1 v2             -- May throw runtime exception in IO (division by 0)
    return (v, env2)
  ```

- Statement sequences
  ```haskell
  execs []     sig env = return (Right env)
  execs (s:ss) sig env = do
    r <- exec s sig env
    case r of
      Left  v    -> return (Left v)
      Right env1 -> execs ss sig env1
  ```

- Statement: ``while`` loop
  ```haskell
  exec (SWhile e s) sig env = do
    (v, env1) <- eval e sig env
    if isFalse v then return (Right env1)
    else execs [SBlock [s], SWhile e s] sig env1
  ```

- Return statement
  ```haskell
  exec (SReturn e) sig env = do
    (v, _) <- eval e sig env
    return (Left v)
  ```

- Function call
  ```haskell
  eval (ECall f es) sig env = do
    (vs, env') <- evals es sig env
    let (Def t _ params ss) = lookupFun f sig
    r <- execs ss sig (makeEnv params vs)
    case r of
      Left  v -> return (v, env')
      Right _ -> runtimeError "Function did not return anything"
  ```

- Value lists
  ```haskell
  evals :: [Exp] -> Sig -> Env -> IO ([Val], Env)
  evals []     sig env = return ([], env)
  evals (e:es) sig env = do
    (v , env1) <- eval  e  sig env
    (vs, env2) <- evals es sig env1
    return (v:vs, env2)
  ```

Using ``evals``, we can hide the state threading in the case of binary operators:
  ```haskell
  eval (EBinOp op e1 e2) sig env = do
    ([v1,v2], env') <- evals [e1,e2] sig env
    v <- binOp op v1 v2
    return (v, env')
  ```

__Remark:__
The matching `[v1,v2]` is total because
the output list of ``evals`` has the same length ``n`` as the input list.
However, GHC does not see that and might complain.
We would need dependent types to communicate the invariant:
  ```haskell
  evals :: Vector Env n -> Sig -> Env -> IO (Vector Val n, Env)
  ```
This is possible in GHC but a bit tricky and clearer in languages like Agda.

The approach so far is a bit _pedestrian_ and slightly error prone.
In particular, we need to be very careful to pass the right
environment to the recursive call when several environments are in
scope.

However, you may be content with this style of implementation, since
everything is explicit and the code clearly says what is going on.

The code though exhibits common programming patterns that are used all
the time in Haskell and can be automated using standard monads and
monad transformers.  Using monad transformers, we can remove some
clutter from our code and hide it behind the standard monad
combinators.

In the remainder of this text, we step-by-step identify the common
programming patterns and how to encapsulate them by monad
transformers.

Monads
------

A _monad_ `m` is a type constructor that implements the `Monad` type class.
```haskell
class Monad m where
  return :: a -> m a
  (>>=)  :: m a -> (a -> m b) -> m b
```
For example, `IO` is a monad, and we assume that the reader is
familiar with the `IO` monad already.

The `return` method embeds a pure computation of type `a` into the monadic computation.
The bind method `ma >>= k` is supposed to run computation `ma` and
feed its result of type `a` to continuation `k`.
The effects of `ma >>= k` are the effects of `ma` and `k a` combined.
We can think of bind in the terms of `do` notation:
```haskell
m >>= k = do
  a <- m
  k a
```
In reality, it is the other way round: `do` is desugared to uses of bind.

A _monad transformer_ `t` takes a monad `m` and makes a new monad `t m`,
usually adding some effects on top of the ones of `m`.

This can be explained best with instances of `Monad`.

Exception monad
---------------

The library `Control.Monad.Except` defines the exception monad
transformer.  In the library, it is a `newtype`, here we just use a
`type` synonym to simplify things.
```haskell
type ExceptT e m a = m (Either e a)
```
A computation in `ExceptT e m a` will return a result of `Either e a`,
where `Left` alternatives of type `e` are considered errors, and
`Right` alternatives of type `a` regular results.  Further, the
effects of the inner monad `m` can happen.  For now, let us think of
`m = IO`, but `m` can be any monad.

The monad implementation embeds pure computations into `Right` and
propagates `Left` results---discarding the continuation `k` when `ma`
produced a `Left`.
```haskell
instance Monad m => Monad (ExceptT e m) where
  return a = return (Right a)
  ma >>= k = do
    r <- ma
    case r of
      Left  e -> return (Left e)
      Right a -> k a
```
We saw this pattern already: in our implementation of `execs`!  Hence,
we can simply `execs`, making use of the exception monad transformer:
```haskell
exec  ::  Stm  -> Sig -> Env -> ExceptT Val IO Env

execs :: [Stm] -> Sig -> Env -> ExceptT Val IO Env
execs []     sig env = return env
execs (s:ss) sig env = do
  env1 <- exec s sig env
  execs ss sig env1
```

Raising and catching of errors is encapsulated in the `MonadError`
class defined in `Control.Monad.Except`:
```haskell
class MonadError e m where
  throwError :: e -> m a
  catchError :: m a -> (e -> m a) -> m a

instance Monad m => MonadError e (ExceptT e m) where
  throwError e    = return (Left e)
  catchError ma h = do
    r <- ma
    case r of
      Left  e -> h e
      Right a -> return (Right a)
```
The `throwError` method returns a non-regular result (`Left`).  The
`catchError` method takes a computation `ma` and looks at its result.
If it is an exception, the handler `h` is invoked.  Otherwise, the
result is passed on.

We use `throwError` in our execution of `SReturn`:
```haskell
exec (SReturn e) sig env = do
  (v, _) <- eval e sig env     -- Problem here!
  throwError v
```
There is a slight problem remaining here: `eval` returns something in
the `IO` monad, but we are now computing in the `ExceptT Val IO`
monad!  We shall deal with this problem shortly.

But first let us observe that we do not really need to catch and
handle errors.  Rather, when we call a function, we want to extract
the exceptional value, and then continue in a monad without
exceptions.  This is facilitated by the `runExceptT` function defined
in `Control.Monad.Except`.
```haskell
runExceptT :: ExceptT e m a -> m (Either e a)
runExceptT = id
```
In our simplification, the run method is the identity, but in the true
implementation, we unwrap the `newtype`:
```haskell
newtype ExceptT e m a = ExceptT { runExceptT :: m (Either e a) }
```

Evaluation of function calls uses `runExceptT`:
```haskell
eval (ECall f es) sig env = do
  (vs, env') <- evals es sig env
  let (Def t _ params ss) = lookupFun f sig
  r <- runExceptT (execs ss sig (makeEnv params vs))
  case r of
    Left  v -> return (v, env')
    Right _ -> runtimeError "Function did not return anything"
```

Monad transformers
------------------

We have observed above the problem of accessing an `IO` computation inside
a `ExceptT Val IO` computation.
Luckily, this problem has been solved by the `MonadIO` class that
gives us access to the `IO` in any monad `m` that supports I/O:
```haskell
class MonadIO m where
  liftIO :: IO a -> m a

instance MonadIO (ExceptT e IO) where
  liftIO io = do
    a <- io
    return (Right a)
```

The method `liftIO` provided by `MonadIO` can be generalized to a
`lift` method that gives access to the functionality of the inner
monad `m` in a transformed monad `t m`.
Module `Control.Monad.Trans` defines a type class `MonadTrans` to this end,
which has an instances for `ExceptT`:
```haskell
class MonadTrans t where
  lift :: Monad m => m a -> t m a

instance MonadTrans (ExceptT e) where
  lift ma = do
    a <- ma
    return (Right a)
```

We can then think of `MonadIO` being implemented simply as:
```haskell
instance MonadIO IO where
  liftIO = id

instance (MonadTrans t, MonadIO m) => MonadIO (t m) where
  liftIO = lift . liftIO
```

We can fix our problem now by using `liftIO` (or, equivalently here, `lift`):
```haskell
exec (SReturn e) sig env = do
  (v, _) <- liftIO (eval e sig env)
  throwError v
```

In general, we may want to think about how to run expression
evaluation inside the statement execution.
```haskell
type Eval = IO
type Exec = ExceptT Val IO

liftEval :: Eval a -> Exec a
liftEval = liftIO

exec (SReturn e) sig env = do
  (v, _) <- liftEval (eval e sig env)
  throwError v
```
While this may look like an over-generalization at the moment and just
boilerplate code, we will refine the definition of `liftEval` as we
introduce more monad transformers.

State monad
-----------

The threading of state, passing in an environment and returning a
modified environment, is a pattern that is encapsulated in the _state
monad transformer_ of `Control.State.Monad`.  Again, dropping the
`newtype` wrapping, the definition would be:
```haskell
type StateT s m a = s -> m (a, s)
```
This equips a monadic computation `m a` with reading from an extra
parameter of type `s`, the input state, and returning an extra result
of type `s`, the output state.

The state threading is taken care of by the `Monad` instance:
```haskell
instance Monad m => Monad (StateT s m) where
  return a = \ s -> return (a, s)
  ma >>= k = \ s -> do
    (a, s') <- ma s
    k a s'
```
The state `s'` returned from `ma` (invoked with the original state
`s`) is passed on to continuation `k`.

We hand-threaded the environment in a couple of places.  All of these
places can now benefit for the encapsulation into the state monad:
```haskell
type Eval = StateT Env IO

evals :: [Exp] -> Sig -> Eval [Val]
evals []     sig = return []
evals (e:es) sig = do
  v  <- eval  e  sig
  vs <- evals es sig
  return (v:vs)

eval :: Exp -> Sig -> Eval Val
eval (EBinOp op e1 e2) sig = do
  v1 <- eval e1 sig
  v2 <- eval e2 sig
  v  <- liftIO $ binOp op v1 v2
  return v
```

The use of `liftIO` requires `StateT s IO` to implement `MonadIO`, or
more generally, `StateT s` to be a monad transformer:
```haskell
instance MonadTrans (StateT s) where
  lift ma = \ s -> do
    a <- ma
    return (a, s)
```

### Accessing the state

Reading and writing to the state is encapsulated in the `MonadState` class.
```haskell
class MonadState s m where
  get    :: m s
  put    :: s -> m ()
  modify :: (s -> s) -> m ()

instance MonadState s (StateT s m) where
  get      = \ s -> return (s, s)
  put s    = \ _ -> return ((), s)
  modify f = \ s -> return ((), f s)
```
These services of the state monad are needed when we access variables.
```haskell
eval (EId x) sig = do
  s <- get
  return $ lookupVar x s

eval (EAssign x e) sig = do
  v <- eval e sig
  modify (\ s -> updateVar x v s)
  return v
```

### Using `StateT` in `exec`

The form of `StateT` does not directly match the pattern of the type of `exec`:
```haskell
exec :: Stm -> Sig -> Env -> ExceptT Val IO Env
```
We need to formally add a result to statement execution.  The result
has no information, though; we use the unit type `()`.
```haskell
exec :: Stm -> Sig -> Env -> ExceptT Val IO ((), Env)
```
Now it fits the form of `StateT`:
```haskell
type Exec = StateT Env (ExceptT Val IO)

exec  ::  Stm  -> Sig -> Exec ()
execs :: [Stm] -> Sig -> Exec ()
```

We can use automatic state threading now also in statement execution:
```haskell
execs []     sig = return ()
execs (s:ss) sig = do
  exec  s  sig
  execs ss sig

exec (SWhile e s) sig = do
  v <- liftEval (eval e sig)
  if isFalse v then return ()
  else execs [SBlock [s], SWhile e s] sig
```

### Using evaluation in execution

The definition of `liftEval` has to be updated:
```haskell
liftEval :: Eval a -> Exec a
liftEval ev = \ s -> do
  (a, s') <- lift (ev s)
  return (a, s')
```
This is simply:
```haskell
liftEval :: Eval a -> Exec a
liftEval ev = \ s -> lift (ev s)
```

However, this definition does not fly with the `newtype` wrapping of `StateT` in `Control.Monad.State`:
```haskell
newtype StateT s m a = StateT { runStateT :: s -> m (a, s) }
```
We need to wrap and unwarp `StateT` in the right places:
```haskell
liftEval :: Eval a -> Exec a
liftEval ev = StateT (\ s -> lift (runStateT ev s))
```
Point-free:
```haskell
liftEval ev = StateT (lift . runStateT ev)
```


Reader monad
------------

Finally, we want to get rid of passing the signature `sig` around
explicitly.  One solution is to make it part of `Env`.  Or, we can
employ the reader monad transformer.
```haskell
type ReaderT r m a = r -> m a
```
This is simpler than the state monad: we only pass a state `r` in,
but do not return one.
```haskell
instance Monad m => Monad (ReaderT r m) where
  return a = \ r -> return a
  ma >>= k = \ r -> do
    a <- ma r
    k a r

instance MonadTrans (ReaderT r) where
  lift ma = \ r -> ma
```
Bind simply distributes the read-only state `r` to both `ma` and `k`.
The embeddings `return` and `lift` simply ignore `r`.

Accessing the state is facilitated through the `MonadReader` class:
```haskell
class MonadReader r m where
  ask   :: m r
  local :: (r -> r) -> m a -> m a

instance MonadReader r (ReaderT r m) where
  ask        = \ r -> return r
  local f ma = \ r -> ma (f r)
```
The method `ask` returns the state, and `local f ma` modifies if via
`f` for a subcomputation `ma`.  Modifying a read-only state seems
paradoxical, but note that the modification only applies to the
continuation `ma`.  Any computation following a `local` in sequence
will receive the original, unmodified state.  We shall not need
`local` in interpreter nor type-checker.

The official definition of `ReaderT` uses a `newtype`:
```haskell
newtype ReaderT r m a = ReaderT { runReaderT :: r -> m a }
```
The projection `runReaderT :: ReaderT r m a -> r -> ma` allows us to
unwind the reader component by supplying an initial state `r`.

The final definitions of the monads for evaluation and execution are thus:
```haskell
type Eval = ReaderT Sig (StateT Env IO)
type Exec = ReaderT Sig (StateT Env (ExceptT Val IO))
```
The lifting of `Eval` computations into `Exec` computations needs
to unwind the reader and state layers and restore them after the `lift` for `ExceptT`.
```haskell
liftEval :: Eval a -> Exec a
liftEval ev = ReaderT (\ r -> StateT (\ s -> lift (runStateT (runReaderT ev r) s)))
```
Its "inverse" is needed to call `execs` from the evaluator of function calls:
```haskell
runExec :: Exec a -> Eval (Either Val a)
```

The idea is that exceptions thrown in a computation `Exec a` should be
transformed into a `Left` result in the `Eval` monad, whereas regular
termination of the computation manifests as `Right` result.  The
problem is that exceptions do not carry the state, because
```haskell
StateT Env (ExceptT Val IO) a = Env -> IO (Either Val (a, Env)).
```
If we look at the relevant part of `Eval (Either Val a)`, this is
```haskell
StateT Env IO (Either Val a) = Env -> IO (Either Val a, Env).
```
So `runExec` needs a state to continue with after the exception.  In
our case, we can even reset the state after a regular termination of
the `Exec a` computation, since we do not need the state of a
function's locals after the function returns.
```haskell
runExecAndReset :: Exec a -> Env -> Eval (Either Val a)
runExecAndReset ex env =
  ReaderT (\ r ->
  StateT  (\ s -> do
    r <- runExceptT (runStateT (runReaderT ex r) s)
    case r of
      Left  v       -> return (Left  v, env)
      Right (a, _s) -> return (Right a, env)))
```
This gives the following implementation of the function call:
```haskell
eval (ECall f es) = do
  vs  <- evals es
  sig <- ask
  let (DFun _ _ params ss) = lookupFun f sig
  env <- get
  put (makeEnv params vs)
  r <- runExecAndReset (execs ss) env
  case r of
    Left v  -> return v
    Right() -> runtimeError "Function did not return anything"
```

Simplification
--------------

We developed `Exec` and `Eval` to capture the patterns found in our
initial solution that used only the `IO` monad.

Of course, it is possible to use the `Exec` monad also for the
evaluator, even though the evaluator never raises an exception.
In this case, there is no need for `liftEval` nor `runExec`.
Instead, we use `catchError` instead of `runExec` in the definition of
`eval (ECall f es)`.

This simplification is recommended, as the resulting interpreter
implementation is easier to understand.

Conclusion
----------

Now that we have completely "monadified" our interpreter,
```haskell
eval :: Exp -> Eval Val
exec :: Stm -> Exec ()
```
what have we gained?
Well, we have removed clutter from our implementation and the chance
of introducing bugs in the boilerplate code.
But further, the lifting of `eval` and `exec` to lists are now completely generic!
```haskell
evals :: [Exp] -> Eval [Val]
evals es = mapM eval es

execs :: [Stm] -> Exec ()
execs ss = mapM_ exec ss
```
It is not even worthwhile anymore to define `evals` and `execs`,
we can simply write `mapM eval` and `mapM_ exec` directly.
