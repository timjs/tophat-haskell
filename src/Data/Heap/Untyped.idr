||| A MonadRef instance with heap and locations indexed by a universe.
module Data.Heap.Untyped


import public Control.Monad.Ref
import        Control.Monad.State
import        Data.Vect
import        Data.Vect.Views


%default total

%hide Language.Reflection.Ref
%hide Language.Reflection.Universe


panic : String -> a
panic = idris_crash



-- Heap ------------------------------------------------------------------------


export
data Loc : (u : Universe t) -> (e : t) -> Type where
  MkLoc : Nat -> Loc u e


||| Heap consisting of a list of type and element pairs over a universe.
Heap : (u : Universe t) -> Type
Heap {t} _ = ( n : Nat ** Vect n (e : t ** typeOf e) )


||| An empty heap over a universe.
empty : Heap u
empty = (0 ** [])


sub : (n : Nat) -> (m : Nat) -> Fin n
sub n m =
  case isLTE m n of
    Yes prf => case natToFin (n - m) n of
      Just k  => k
      Nothing => panic "Index is out of bounds, this should not happen"
    No cont => panic "Index is bigger than vector length, this should not happen"


extend : Monad m => {u : Universe t} -> typeOf e -> StateT (Heap u) m (Loc u e)
extend {e} x = do
  ( n ** xs ) <- get
  put ( S n ** ( e ** x ) :: xs )
  pure $ MkLoc n


update : Monad m => {u : Universe t} -> Loc u e -> typeOf e -> StateT (Heap u) m ()
update (MkLoc {e} i) y = do
  ( n ** xs ) <- get
  put $ ( n ** Vect.replaceAt (sub n i) ( e ** y ) xs )


lookup : Monad m => {u : Universe t} -> Loc u e -> StateT (Heap u) m (typeOf e)
lookup (MkLoc {e} i) = do
    ( n ** xs ) <- get
    let ( e' ** x ) = Vect.index (sub n i) xs
    case decEq e e' of
      Yes Refl => pure x
      No contr => panic "Types of location and cell do not match, this should not happen"



-- Transformer -----------------------------------------------------------------


export
data RefT : (u : Universe t) -> (m : Type -> Type) -> (a : Type) -> Type where
  Pure  : (x : a) -> RefT u m a
  Bind  : (this : RefT u m a) -> (next : a -> RefT u m b) -> RefT u m b

  New   : (x : typeOf @{u} e) -> RefT u m (Loc u e)
  Read  : (l : Loc u e) -> RefT u m (typeOf @{u} e)
  Write : (l : Loc u e) -> (x : typeOf @{u} e) -> RefT u m ()


public export
Ref : (u : Universe t) -> (a : Type) -> Type
Ref u = RefT u Identity



-- Running ---------------------------------------------------------------------


export
runRefT : Monad m => RefT u m a -> m a
runRefT r = fst <$> runStateT (go r) empty
where
  go : Monad m => RefT u m a -> StateT (Heap u) m a
  go (Pure x) =
      pure x
  go (Bind this next) = do
      x <- go this
      go $ next x
  go (New x) = do
      l <- extend x
      pure l
  go (Read l) = do
      x <- lookup l
      pure x
  go (Write l x) = do
      update l x



-- Interfaces ------------------------------------------------------------------
--NOTE: These are just the free monad implementations...


Functor (RefT u m) where
    map f m = Bind m (Pure . f)


Applicative (RefT u m) where
    pure = Pure

    f <*> x = Bind f (\f => map f x)


Monad (RefT u m) where
    (>>=) = Bind


MonadRef t (Loc u) (RefT u m) where
    ref    = New
    deref  = Read
    assign = Write


{-------------------------------------------------------------------------------

-------------------------------------------------------------------------------}
