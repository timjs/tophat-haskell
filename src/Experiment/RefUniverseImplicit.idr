module Control.Monad.Ref

import public Control.Monad.Identity
import public Control.Monad.Trans
import public Data.IORef
import public Data.Universe

%default total
%access public export

%hide Language.Reflection.Ref
%hide Language.Reflection.Universe



-- Interface -------------------------------------------------------------------


interface Monad m => MonadRef (u : Universe t) (l : t -> Type) (m : Type -> Type) | m where
  ref    : (typeOf a) -> m (l a)
  deref  : l a -> m (typeOf a)
  assign : l a -> (typeOf a) -> m ()


infix 4 :=
(:=) : MonadRef u l m => l a -> (typeOf @{u} a) -> m ()
(:=) = assign



-- Instances -------------------------------------------------------------------


-- MonadRef u IORef IO where
--   ref    = newIORef
--   deref  = readIORef
--   assign = writeIORef


-- Helpers ---------------------------------------------------------------------

modify : MonadRef u l m => l a -> (typeOf @{u} a -> typeOf @{u} a) -> m ()
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
