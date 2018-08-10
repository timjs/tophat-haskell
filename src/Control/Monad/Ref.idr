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


||| ...
|||
||| The way to go is to parametrise `MonadRef` over a type `t` which has the
||| interface constraint of `Universe`.
||| Now `t` is an currated type containing a way to translate its elements `e`
||| to real Idris types with the `typeOf` method.
||| The interface record will get passed along automatically.
interface Monad m => MonadRef (u : Universe t) (l : t -> Type) (m : Type -> Type) | m where
  ref    : typeOf e -> m (l e)
  deref  : l e -> m (typeOf e)
  assign : l e -> typeOf e -> m ()


infix 4 :=
(:=) : (u : Universe t) => MonadRef u l m => l e -> (typeOf e) -> m ()
(:=) = assign



-- Instances -------------------------------------------------------------------


IORefU : {u : Universe t} -> (e : t) -> Type
IORefU {u} e = IORef (typeOf @{u} e)


||| Note: this is a restricted usage of `IORef` over a universe `u`!
MonadRef u (IORefU {u}) IO where
  ref    = newIORef
  deref  = readIORef
  assign = writeIORef



-- Helpers ---------------------------------------------------------------------


modify : (u : Universe t) => MonadRef u l m => l e -> (typeOf e -> typeOf e) -> m ()
modify l f = do
  x <- deref l
  l := f x
