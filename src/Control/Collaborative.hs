module Control.Collaborative
  ( Collaborative(..), (<<-), (<<=)
  , Someref, pack, unpack
  ) where

import Data.Editable


-- | A monad with `Editable` reference cells and pointer equality.
class ( Monad m, Typeable l, forall a. Eq (l a) ) => Collaborative l m | m -> l where
  store  :: Editable a => a -> m (l a)

  assign :: Editable a => l a -> a -> m ()

  watch  :: Editable a => l a -> m a

  change :: Editable a => l a -> m a
  change = watch

infixl 1 <<-
infixl 1 <<=

(<<-) :: Collaborative l m => Editable a => l a -> a -> m ()
(<<-) = assign

(<<=) :: Collaborative l m => Editable a => l a -> (a -> a) -> m ()
(<<=) l f = do
  x <- watch l
  assign l (f x)

instance ( Collaborative l m, Monoid w ) => Collaborative l (WriterT w m) where
  store    = lift << store
  assign l = lift << assign l
  watch    = lift << watch

instance Collaborative IORef IO where
  store  = newIORef
  assign = writeIORef
  watch  = readIORef


-- Existential packing ---------------------------------------------------------


data Someref (m :: Type -> Type) where
  Someref :: ( Collaborative l m, Typeable (l a), Eq (l a) ) => l a -> Someref m


pack :: forall m l a. ( Collaborative l m, Typeable (l a), Eq (l a) ) => l a -> Someref m
pack = Someref


unpack :: forall m l a. ( Typeable (l a) ) => Someref m -> Maybe (l a)
unpack (Someref x)
  | Just Refl <- typeOf x ~~ r = Just x
  | otherwise = Nothing
  where
    r = typeRep :: TypeRep (l a)


instance Eq (Someref m) where
  Someref x == Someref y
    | Just Refl <- x ~= y = x == y
    | otherwise = False
