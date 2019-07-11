module Control.Locative
  ( Locative(..), (<<-)
  , Someref, pack, unpack
  ) where

import Data.Editable


infixl 1 <<-

-- | A monad with `Editable` reference cells and pointer equality.
class ( Monad m, Typeable l, forall a. Eq (l a) ) => Locative l m | m -> l where
  ref    :: Editable a => a -> m (l a)
  deref  :: Editable a => l a -> m a
  assign :: Editable a => l a -> a -> m ()

(<<-) :: Locative l m => Editable a => l a -> a -> m ()
(<<-) = assign

-- modify :: Locative l m => Editable a => l a -> (a -> a) -> m ()
-- modify l f = do
--   x <- deref l
--   assign l (f x)

instance Locative IORef IO where
  ref    = newIORef
  deref  = readIORef
  assign = writeIORef



-- Existential packing ---------------------------------------------------------


data Someref (m :: Type -> Type) where
  Someref :: ( Locative l m, Typeable (l a), Eq (l a) ) => l a -> Someref m


pack :: forall m l a. ( Locative l m, Typeable (l a), Eq (l a) ) => l a -> Someref m
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
