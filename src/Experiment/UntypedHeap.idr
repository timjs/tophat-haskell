||| A MonadRef instance with heap and locations indexed by a universe.
|||
||| Note:
||| - We can't make an instance `MonadRef (Loc ts) (RefT ts ts' m)`
|||   because `Loc ts : Ty u -> Type` instead of `Type -> Type`...
module Control.Monad.Ref.TypedHeap


import public Control.Monad.Ref
import        Data.Vect
import public Data.UniverseR


%default total
%access public export

%hide Language.Reflection.Ref
%hide Language.Reflection.Universe



-- Implementation --------------------------------------------------------------

-- Types --

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
