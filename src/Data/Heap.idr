||| A MonadRef instance with heap and locations indexed by a universe.
|||
||| Note:
||| - We can't make an instance `MonadRef (Loc es) (RefT es es' m)`
|||   because `Loc es : Ty u -> Type` instead of `Type -> Type`...
module Experiment.TypedHeap


import public Experiment.RefUniverseConstraint


%default total
%access public export

%hide Language.Reflection.Ref
%hide Language.Reflection.Universe



-- Implementation --------------------------------------------------------------

-- Types --


||| The shape of a heap.
|||
||| Determines the type of data stored at every memory location.
||| Heaps have a length `k` and every location has a certain type `t` from the universe `u`.
Shape : Universe t => Type
Shape {t} = List t


||| Hetrogenious heap indexed by a `Shape`.
|||
||| `Nil` is the empty heap.
||| `(::)` allocates a value `a` of type `typeOf e` on a heap `as` of shape `es`
||| to construct a heap of shape `e :: es`.
|||
||| Note: corresponds to an universe indexed variant of `Data.HVect`.
data Heap : (u : Universe t) => (es : Shape @{u}) -> Type where
  Nil  : Heap @{u} []
  (::) : (a : typeOf @{u} e) -> (as : Heap @{u} es) -> Heap @{u} (e :: es)


||| Location on the heap.
|||
||| A value of type `Loc es e` corresponds to a proof that a value of type `typeOf e`
||| is stored in a heap of shape `es`.
||| `Here` means it is stored at this location.
||| `There` means it is stored a bit further on the heap.
|||
||| Note: corresponds to an universe indexed variant of `Data.Vect.Elem`.
data Loc : (u : Universe t) => (es : Shape @{u}) -> (e : t) -> Type where
  Here  : Loc @{u} (e :: es) e
  There : (later : Loc @{u} es e) -> Loc @{u} (e' :: es) e


data RefT : (u : Universe t) => (es : Shape @{u}) -> (es' : Shape @{u}) -> (m : Type -> Type) -> (a : Type) -> Type where
  Pure  : (x : a) -> RefT @{u} es es m a
  Bind  : (this : RefT @{u} es es' m a) -> (next : a -> RefT @{u} es' es'' m b) -> RefT @{u} es es'' m b

  New   : (x : typeOf @{u} e) -> RefT @{u} es (e :: es) m (Loc @{u} (e :: es) e)
  Read  : (l : Loc @{u} es e) -> RefT @{u} es es m (typeOf @{u} e)
  Write : (l : Loc @{u} es e) -> (x : typeOf @{u} e) -> RefT @{u} es es m ()


Ref : (u : Universe t) => (es : Shape @{u}) -> (es' : Shape @{u}) -> (a : Type) -> Type
Ref es es' = RefT es es' Identity



-- Semantics --


lookup : Universe t => Loc es e -> Heap es -> typeOf {t} e
lookup _         []        impossible
lookup (Here)    (x :: _)  = x
lookup (There l) (_ :: xs) = lookup l xs


update : Universe t => Loc es e -> typeOf {t} e -> Heap es -> Heap es
update _         _ []        impossible
update (Here)    y (_ :: xs) = y :: xs
update (There l) y (x :: xs) = x :: update l y xs


runT : Universe t => Monad m => RefT {t} es es' m a -> Heap es -> m ( a, Heap es' )
runT (Pure x)         xs = pure ( x, xs )
runT (Bind this next) xs = do
  ( this, xs' ) <- runT this xs
  runT (next this) xs'
runT (New x)          xs = pure ( Here, x :: xs )
runT (Read l)         xs = pure ( lookup l xs, xs )
runT (Write l x)      xs = pure ( (), update l x xs )


run : Universe t => Ref {t} es es' a -> Heap es -> ( a, Heap es' )
run r = runIdentity . runT r


eval : Universe t => Ref {t} es es' a -> Heap es -> a
eval mut = fst . run mut


exec : Universe t => Ref {t} es es' a -> Heap es -> Heap es'
exec mut = snd . run mut



-- Instances -------------------------------------------------------------------


( Universe t, Functor f ) => Functor (RefT {t} es es' f) where
  map g (Pure x)         = ?holeFunctor_1
  map g (Bind this next) = ?holeFunctor_2
  map g (New x)          = ?holeFunctor_3
  map g (Read l)         = ?holeFunctor_4
  map g (Write l x)      = ?holeFunctor_5


( Universe t, Applicative f ) => Applicative (RefT {t} es es' f) where
  --XXX: we can't implement pure because of the type indices :-(
  pure = ?holeApplicatePure

  f <*> fa = ?holeApplyApplicative


( Universe t, Monad m ) => Monad (RefT {t} es es' m) where
  (Pure x)         >>= k = ?holeMonadBind_1
  (Bind this next) >>= k = ?holeMonadBind_2
  (New x)          >>= k = ?holeMonadBind_3
  (Read l)         >>= k = ?holeMonadBind_4
  (Write l x)      >>= k = ?holeMonadBind_5


( Universe t, Monad m ) => MonadRef t (Loc es) (RefT {t} es es' m) where
  ref x = ?holeMonadRefRef
  deref l = ?holeMonadRefDeref
  assign l x = ?holeMonadRefAssign

-------------------------------------------------------------------------------}
