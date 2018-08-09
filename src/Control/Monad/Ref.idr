module Control.Monad.Ref

import public Control.Monad.Identity
import public Control.Monad.Trans
import public Data.Universe

%default total
%access public export

%hide Language.Reflection.Ref
%hide Language.Reflection.Universe



-- Interface -------------------------------------------------------------------


||| ...
|||
||| The way to go is to parametrise `MonadRef` over a type `t` which has the
||| interface constraint of `Universe`.
||| Now `t` is an currated type containing a way to translate its elements `e`
||| to real Idris types with the `typeOf` method.
||| The interface record will get passed along automatically.
interface ( Monad m, Universe t ) => MonadRef (t : Type) (l : t -> Type) (m : Type -> Type) | m where
  ref    : typeOf a -> m (l a)
  deref  : l a -> m (typeOf a)
  assign : l a -> typeOf a -> m ()


infix 4 :=
(:=) : MonadRef t l m => l a -> (typeOf a) -> m ()
(:=) = assign



-- Instances -------------------------------------------------------------------


||| Note:
||| - We can't make an instance of MonadRef for IO because of the type clash between
|||   `l : t -> Type` and `IORef : Type -> Type`
-- MonadRef t IORef IO where
--   ref    = newIORef
--   deref  = readIORef
--   assign = writeIORef


-- Helpers ---------------------------------------------------------------------

modify : MonadRef t l m => l a -> (typeOf a -> typeOf a) -> m ()
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
