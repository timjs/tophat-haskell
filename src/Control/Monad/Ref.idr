module Control.Monad.Ref

import public Control.Monad.Identity
import public Control.Monad.Trans
import public Data.IORef
import public Data.Universe

%default total
%access public export


-- Interface -------------------------------------------------------------------

interface Monad m => MonadRef (l : Type -> Type) (m : Type -> Type) | m where
  ref    : a -> m (l a)
  deref  : l a -> m a
  assign : l a -> a -> m ()

infix 4 :=
(:=) : MonadRef l m => l a -> a -> m ()
(:=) = assign


-- Implementation --------------------------------------------------------------

data Cell : {u : Universe t} -> (a : t) -> Type where
  MkCell : Nat -> Cell a

data RefT : {u : Universe t} -> (m : Type -> Type) -> (a : t) -> Type where
  Pure  : (x : typeOf @{u} a) -> RefT m a
  Bind  : (this : RefT m a) -> (next : a -> RefT m b) -> RefT m b

  New   : (x : typeOf @{u} a) -> RefT m (Cell a)
  Read  : (l : Cell a) -> RefT m a
  Write : (l : Cell a) -> (x : typeOf @{u} a) -> RefT m ()

Ref : {u : Universe t} -> (a : t) -> Type
Ref {u} = RefT {u} Identity


-- Instances -------------------------------------------------------------------

MonadRef IORef IO where
  ref    = newIORef
  deref  = readIORef
  assign = writeIORef


-- Helpers ---------------------------------------------------------------------

modify : MonadRef l m => l a -> (a -> a) -> m ()
modify l f = do
  x <- deref l
  l := f x


{- Tests -----------------------------------------------------------------------

test0 : IO Int
test0 = do
  r <- newIORef 5
  writeIORef r 10
  x <- readIORef r
  pure x

test : MonadRef IORef IO => IO Int
test = do
  r <- ref (the Int 5)
  r := (the Int 10)
  x <- deref r
  pure x

-------------------------------------------------------------------------------}
