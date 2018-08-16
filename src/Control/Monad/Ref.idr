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


modify : MonadRef l m => l a -> (a -> a) -> m ()
modify l f = do
  x <- deref l
  l := f x
