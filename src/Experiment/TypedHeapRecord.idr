||| A MonadRef instance with heap and locations indexed by a universe.
|||
||| Note:
||| - We can't make an instance `MonadRef (Loc es) (RefT es es' m)`
|||   because `Loc es : Ty u -> Type` instead of `Type -> Type`...
module Experiment.TypedHeap


import public Experiment.RefUniverseRecord


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
Shape : {u : Universe} -> Type
Shape {u} = List (Ty u)


||| Hetrogenious heap indexed by a `Shape`.
|||
||| `Nil` is the empty heap.
||| `(::)` allocates a value `a` of type `typeOf e` on a heap `as` of shape `es`
||| to construct a heap of shape `e :: es`.
|||
||| Note: corresponds to an universe indexed variant of `Data.HVect`.
data Heap : (es : Shape) -> Type where
  Nil  : Heap []
  (::) : (a : typeOf u e) -> (as : Heap es) -> Heap (e :: es)


||| Location on the heap.
|||
||| A value of type `Loc es e` corresponds to a proof that a value of type `typeOf e`
||| is stored in a heap of shape `es`.
||| `Here` means it is stored at this location.
||| `There` means it is stored a bit further on the heap.
|||
||| Note: corresponds to an universe indexed variant of `Data.Vect.Elem`.
data Loc : (es : Shape) -> (e : Ty u) -> Type where
  Here  : Loc (e :: es) e
  There : (later : Loc es e) -> Loc (e' :: es) e


data RefT : (es : Shape) -> (es' : Shape) -> (m : Type -> Type) -> (a : Type) -> Type where
  Pure  : (x : a) -> RefT es es m a
  Bind  : (this : RefT es es' m a) -> (next : a -> RefT es' es'' m b) -> RefT es es'' m b

  New   : (x : typeOf u e) -> RefT es (e :: es) m (Loc (e :: es) e)
  Read  : (l : Loc es e) -> RefT es es m (typeOf u e)
  Write : (l : Loc es e) -> (x : typeOf u e) -> RefT es es m ()


Ref : (es : Shape) -> (es' : Shape) -> (a : Type) -> Type
Ref es es' = RefT es es' Identity



-- Semantics --


lookup : Loc es e -> Heap es -> typeOf u e
lookup _         []        impossible
lookup (Here)    (x :: _)  = ?h
lookup (There l) (_ :: xs) = lookup l xs


update : Loc es e -> typeOf u e -> Heap es -> Heap es
update _         _ []        impossible
update (Here)    y (_ :: xs) = y :: xs
update (There l) y (x :: xs) = x :: update l y xs


runT : Monad m => RefT es es' m a -> Heap es -> m ( a, Heap es' )
runT (Pure x)         xs = pure ( x, xs )
runT (Bind this next) xs = do
  ( this, xs' ) <- runT this xs
  runT (next this) xs'
runT (New x)          xs = pure ( Here, x :: xs )
runT (Read l)         xs = pure ( lookup l xs, xs )
runT (Write l x)      xs = pure ( (), update l x xs )


run : Ref es es' a -> Heap es -> ( a, Heap es' )
run r = runIdentity . runT r


eval : Ref es es' a -> Heap es -> a
eval mut = fst . run mut


exec : Ref es es' a -> Heap es -> Heap es'
exec mut = snd . run mut


{-------------------------------------------------------------------------------

-- Instances -------------------------------------------------------------------


Functor f => Functor (RefT es es' f) where
  map f fa = ?holeFunctor


Applicative f => Applicative (RefT es es' f) where
  pure a = ?holePureApplicative

  f <*> fa = ?holeApplyApplicative


Monad m => Monad (RefT es es' m) where
  fa >>= f = ?holeMonadBind


-- Monad m => MonadRef u (Loc es) (RefT es es' m) where
--   ref x = ?holeMonadRefRef
--   deref l = ?holeMonadRefDeref
--   assign l x = ?holeMonadRefAssign

-------------------------------------------------------------------------------}
