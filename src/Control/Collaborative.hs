module Control.Collaborative
  ( module Data.Store,
    Collaborative (..),
    (<<-),
    (<<=),
    Someref,
    pack,
    unpack,
  )
where

import Data.Editable
import Data.Store
import qualified Lens.Simple as Lens

-- Class -----------------------------------------------------------------------

-- | A variation on a monad `m` with references `r`.
-- |
-- | References `r` should be typeable, so they can be existentially stored.
-- | Also, they should be equatable, so we can compare the pointers to check if they are the same.
-- | Data `a` stored in such references should be editable,
-- | so they can be shown in an editor on screen and edited by an end user.
-- |
-- | NOTE: GHC does not let us write this as a type synonym,
-- | because of the impredicativity of `Eq (r a)`.
class (Monad m, Typeable r, forall a. Eq (r a)) => Collaborative r m | m -> r where
  share :: Editable a => a -> m (Store r a)
  watch :: Editable a => Store r a -> m a
  assign :: Editable a => a -> Store r a -> m ()

  change :: Editable a => Store r a -> m a
  change = watch

  mutate :: Editable a => (a -> a) -> Store r a -> m ()
  mutate f r = do
    x <- watch r
    assign (f x) r

-- Operators -------------------------------------------------------------------

infixl 1 <<-

infixl 1 <<=

(<<-) :: (Collaborative r m, Editable a) => Store r a -> a -> m ()
(<<-) = flip assign

(<<=) :: (Collaborative r m, Editable a) => Store r a -> (a -> a) -> m ()
(<<=) = flip mutate

-- Instances -------------------------------------------------------------------

instance (Collaborative r m, Monoid w) => Collaborative r (WriterT w m) where
  share = lift << share
  assign r = lift << assign r
  watch = lift << watch

instance (Collaborative r m) => Collaborative r (ExceptT e m) where
  share = lift << share
  assign r = lift << assign r
  watch = lift << watch

instance Collaborative IORef IO where
  share x = do
    r <- newIORef x
    pure <| Store _identity r

  assign x (Store l r) = do
    modifyIORef r (Lens.set l x)

  watch (Store l r) = do
    s <- readIORef r
    pure <| Lens.view l s

-- Existential packing ---------------------------------------------------------

data Someref (m :: Type -> Type) where
  Someref :: (Collaborative r m, Typeable (r a), Eq (r a)) => r a -> Someref m

pack :: forall m r a. (Collaborative r m, Typeable (r a), Eq (r a)) => r a -> Someref m
pack = Someref

unpack :: forall m r a. (Typeable (r a)) => Someref m -> Maybe (r a)
unpack (Someref x)
  | Just Refl <- typeOf x ~? r = Just x
  | otherwise = Nothing
  where
    r = typeRep :: TypeRep (r a)

instance Eq (Someref m) where
  Someref x == Someref y
    | Just Refl <- x ~= y = x == y
    | otherwise = False
