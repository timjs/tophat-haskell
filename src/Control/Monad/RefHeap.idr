module Control.Monad.RefHeap


import public Control.Monad.Identity
import public Control.Monad.Trans
import        Data.Vect
import public Data.IORef
import public Data.UniverseR


%default total
%access public export

%hide Language.Reflection.Ref
%hide Language.Reflection.Universe



-- Interface -------------------------------------------------------------------


interface Monad m => MonadRef (l : Type -> Type) (m : Type -> Type) | m where
  ref    : a -> m (l a)
  deref  : l a -> m a
  assign : l a -> a -> m ()


infix 4 :=
(:=) : MonadRef l m => l a -> a -> m ()
(:=) = assign



-- Implementation --------------------------------------------------------------

-- Types --


||| The shape of a heap.
|||
||| Determines the type of data stored at every memory location.
||| Heaps have a length `k` and every location has a certain type `t` from the universe `u`.
Shape : {u : Universe} -> (k : Nat) -> Type
Shape {u} k = Vect k (Ty u)


||| Hetrogenious heap indexed by a `Shape`.
|||
||| `Nil` is the empty heap.
||| `(::)` allocates a value `a` of type `typeOf t` on a heap `as` of shape `ts`
||| to construct a heap of shape `t :: ts`.
|||
||| Note: corresponds to an universe indexed variant of `Data.HVect`.
data Heap : (ts : Shape k) -> Type where
  Nil  : Heap []
  (::) : (a : typeOf u t) -> (as : Heap ts) -> Heap (t :: ts)


||| Location on the heap.
|||
||| A value of type `Loc t s` corresponds to a proof that a value of type `typeOf t`
||| is stored in a heap of shape `s`.
||| `Here` means it is stored at this location.
||| `There` means it is stored a bit further on the heap.
|||
||| Note: corresponds to an universe indexed variant of `Data.Vect.Elem`.
data Loc : (t : Ty u) -> (ts : Shape k) -> Type where
  Here  : Loc t (t :: ts)
  There : (later : Loc t ts) -> Loc t (s :: ts)


data RefT : (ts : Shape k) -> (ts' : Shape k') -> (m : Type -> Type) -> (a : Type) -> Type where
  Pure  : (x : a) -> RefT ts ts m a
  Bind  : (this : RefT ts ts' m a) -> (next : a -> RefT ts' ts'' m b) -> RefT ts ts'' m b

  New   : (x : typeOf u t) -> RefT ts (t :: ts) m (Loc t (t :: ts))
  Read  : (l : Loc t ts) -> RefT ts ts m (typeOf u t)
  Write : (l : Loc t ts) -> (x : typeOf u t) -> RefT ts ts m ()


Ref : (ts : Shape k) -> (ts' : Shape k') -> (a : Type) -> Type
Ref ts ts' = RefT ts ts' Identity



-- Semantics --


lookup : Loc t ts -> Heap ts -> typeOf u t
lookup _         []        impossible
lookup (Here)    (x :: _)  = x
lookup (There l) (_ :: xs) = lookup l xs


update : Loc t ts -> typeOf u t -> Heap ts -> Heap ts
update _         _ []        impossible
update (Here)    y (_ :: xs) = y :: xs
update (There l) y (x :: xs) = x :: update l y xs


runT : Monad m => RefT ts ts' m a -> Heap ts -> m ( a, Heap ts' )
runT (Pure x)         xs = pure ( x, xs )
runT (Bind this next) xs = do
  ( this, xs' ) <- runT this xs
  runT (next this) xs'
runT (New x)          xs = pure ( Here, x :: xs )
runT (Read l)         xs = pure ( lookup l xs, xs )
runT (Write l x)      xs = pure ( (), update l x xs )


run : Ref ts ts' a -> Heap ts -> ( a, Heap ts' )
run r = runIdentity . runT r


eval : Ref ts ts' a -> Heap ts -> a
eval mut = fst . run mut


exec : Ref ts ts' a -> Heap ts -> Heap ts'
exec mut = snd . run mut



{-------------------------------------------------------------------------------
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



-- Tests -----------------------------------------------------------------------


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
