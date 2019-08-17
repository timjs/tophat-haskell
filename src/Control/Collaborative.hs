module Control.Collaborative
  ( module Data.Store
  , Collaborative(..), (<<-), (<<=)
  , Someref, pack, unpack
  ) where

import Control.Interactive
import Data.Editable
import Data.Store
import qualified Lens.Simple as Lens

-- | A monad with `Editable` reference cells and pointer equality.
class ( Interactive m, Typeable r, forall a. Eq (r a) ) => Collaborative r m | m -> r where
  share  :: ( Editable a ) => a -> m (Store r a a)
  assign :: ( Editable a, Editable s ) => Store r s a -> a -> m ()
  watch  :: ( Editable a, Editable s ) => Store r s a -> m a
  change :: ( Editable a, Editable s ) => Store r s a -> m a

infixl 1 <<-
infixl 1 <<=

(<<-) ::
  Collaborative r m => Editable a => Editable s =>
  Store r s a -> a -> m ()
(<<-) = assign

(<<=) ::
  Collaborative r m => Editable a => Editable s =>
  Store r s a -> (a -> a) -> m ()
(<<=) r f = do
  x <- watch r
  assign r (f x)

instance ( Collaborative r m, Monoid w ) => Collaborative r (WriterT w m) where
  share    = lift << share
  assign r = lift << assign r
  watch    = lift << watch
  change   = lift << change

instance ( Collaborative r m ) => Collaborative r (ExceptT e m) where
  share    = lift << share
  assign r = lift << assign r
  watch    = lift << watch
  change   = lift << change

instance Collaborative IORef IO where
  share x = do
    r <- newIORef x
    pure <| Store _identity r

  assign (Store l r) x = do
    modifyIORef r (Lens.set l x)

  watch (Store l r) = do
    s <- readIORef r
    let x = Lens.view l s
    view x

  change s = do
    _ <- watch s
    x <- enter
    assign s x
    pure x



-- Existential packing ---------------------------------------------------------


data Someref (m :: Type -> Type) where
  Someref :: ( Collaborative r m, Typeable (r a), Eq (r a) ) => r a -> Someref m


pack :: forall m r a. ( Collaborative r m, Typeable (r a), Eq (r a) ) => r a -> Someref m
pack = Someref


unpack :: forall m r a. ( Typeable (r a) ) => Someref m -> Maybe (r a)
unpack (Someref x)
  | Just Refl <- typeOf x ~~ r = Just x
  | otherwise = Nothing
  where
    r = typeRep :: TypeRep (r a)


instance Eq (Someref m) where
  Someref x == Someref y
    | Just Refl <- x ~= y = x == y
    | otherwise = False
