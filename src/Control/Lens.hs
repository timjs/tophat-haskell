module Control.Lens where

import Data.Functor.Constant


-- Pure lenses -----------------------------------------------------------------

type Lens s t a b = forall f. ( Functor f ) => (a -> f b) -> s -> f t
type Lens' s a = Lens s s a a

lens :: (s -> a) -> (s -> b -> t) -> Lens s t a b
lens getting setting f x = map (setting x) (f (getting x))

get :: Lens s t a b -> (s -> a)
get l = getConstant << l Constant

set :: Lens s t a b -> s -> b -> t
set l s v = runIdentity <| l (const <| Identity v) s


-- Monadic lenses --------------------------------------------------------------

type Focus m s a b = forall f. ( Traversable f ) => (a -> f b) -> s -> m (f ())
type Focus' m s a = Focus m s a a

focus :: ( Monad m ) => (s -> m a) -> (s -> b -> m ()) -> Focus m s a b
focus reading writing f x = reading x >>= traverse (writing x) << f

read :: ( Monad m ) => Focus m s a b -> s -> m a
read l s = map getConstant <| l Constant s

write :: ( Monad m ) => Focus m s a b -> s -> b -> m ()
write l s v = map runIdentity <| l (const <| Identity v) s
