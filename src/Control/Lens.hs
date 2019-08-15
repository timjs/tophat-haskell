module Control.Lens where

import Data.Functor.Constant


-- Pure lenses -----------------------------------------------------------------

type Lens s t a b = forall f. ( Functor f ) => (a -> f b) -> s -> f t
type Lens' s a = Lens s s a a

lens :: (s -> a) -> (s -> b -> t) -> Lens s t a b
lens getting setting f x = map (setting x) (f (getting x))

iso :: (a -> b) -> (b -> a) -> Lens' a b
iso f g = lens f (\_ x -> g x)

value :: Lens' a a
value = iso identity identity

get :: Lens s t a b -> (s -> a)
get l = getConstant << l Constant

set :: Lens s t a b -> b -> s -> t
set l v s = runIdentity <| l (const <| Identity v) s

-- Examples --

_1 :: Lens (a, c) (b, c) a b
_1 f ~(a,b) = map (\a' -> ( a',b )) (f a)

_2 :: Lens (c, a) (c, b) a b
_2 f ~(a,b) = map (\b' -> ( a,b' )) (f b)

double :: Lens' Double Double
double f n = map (\n' -> n' / 2) (f (n * 2))

double' :: Lens' Double Double
double' = lens (\n -> n * 2) (\_ n -> n / 2)

double'' :: Lens' Double Double
double'' = iso ((*) 2) ((/) 2)


-- Monadic lenses --------------------------------------------------------------

type LensM m s a b = forall f. ( Traversable f ) => (a -> f b) -> s -> m (f ())
type LensM' m s a = LensM m s a a

lensM :: ( Monad m ) => (s -> m a) -> (s -> b -> m ()) -> LensM m s a b
lensM reading writing f x = reading x >>= traverse (writing x) << f

getM :: ( Monad m ) => LensM m s a b -> s -> m a
getM l s = map getConstant <| l Constant s

setM :: ( Monad m ) => LensM m s a b -> b -> s -> m ()
setM l v s = map runIdentity <| l (const <| Identity v) s


-- Read/Write lenses -----------------------------------------------------------

type ReadWriteLens s a = LensM' IO (IORef s) a

into :: ReadWriteLens a a
into = lensM readIORef (writeIORef)

into1 :: ReadWriteLens ( a, b ) a
into1 = into << _1

into2 :: ReadWriteLens ( a, b ) b
into2 = into << _2

pair :: IO (IORef ( Text, Int ))
pair = newIORef ( "Hallo", 42 )

test :: IO Int
test = do
  r <- pair
  getM into2 r
