module Control.Monad.Ref
  ( MonadRef(..), (<<-)
  -- , Someref, pack, unpack
  ) where

import Data.Editable


infixl 7 <<-

class Monad m => MonadRef l m | m -> l where
  ref    :: Storable a => a -> m (l a)
  deref  :: Storable a => l a -> m a
  assign :: Storable a => l a -> a -> m ()

(<<-) :: MonadRef l m => Storable a => l a -> a -> m ()
(<<-) = assign

-- modify :: MonadRef l m => Storable a => l a -> (a -> a) -> m ()
-- modify l f = do
--   x <- deref l
--   assign l (f x)

instance MonadRef IORef IO where
  ref    = newIORef
  deref  = readIORef
  assign = writeIORef



{- Existential packing ---------------------------------------------------------


data Someref (m :: Type -> Type) where
  Someref :: MonadRef l m => TypeRep (l a) -> l a -> Someref m


pack :: forall m l a. ( MonadRef l m, Typeable a ) => l a -> Someref m
pack = Someref typeRep


unpack :: forall m l a. ( MonadRef l m, Typeable a ) => Someref m -> Maybe (l a)
unpack (Someref r x)
  | Just Refl <- r ~~ r' = Just x
  | otherwise = Nothing
  where
    r' = typeRep :: TypeRep (l a)


instance Eq (Someref m) where
  Someref rx x == Someref ry y
    | Just Refl <- rx ~~ ry = x == y
    | otherwise = False

-}
