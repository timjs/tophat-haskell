module Control.Monad.Ref

import public Control.Monad.Identity
import public Control.Monad.Trans
import public Data.IORef

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


-- Instances -------------------------------------------------------------------

MonadRef IORef IO where
  ref    = newIORef
  deref  = readIORef
  assign = writeIORef


-- Helpers ---------------------------------------------------------------------

update : MonadRef l m => l a -> (a -> a) -> m ()
update loc f = ?monad_ref_update


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
