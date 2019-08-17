module Data.Store
  ( Store(..), Store'
  , focus, _identity
  ) where

import Lens.Simple (Lens', iso)


-- Stores ----------------------------------------------------------------------

-- NOTE:
-- I think the source and target types always have to be the same,
-- otherwise you would be able to change the type of the inner reference...
-- Isn't it?
data Store r s a = Store (Lens' s a) (r s)
type Store' r a = Store r a a

{-
share :: ( MonadRef r m ) => a -> m (Share r a a)
share x = do
  r <- new x
  pure <| Share _identity r

watch :: ( MonadRef r m ) => Share r s a -> m a
watch (Share l r) = do
  s <- read r
  pure <| view l s

assign :: ( MonadRef r m ) => a -> Share r s a -> m ()
assign x (Share l r) = do
  r <<= set l x
-}


-- Focussing -------------------------------------------------------------------

_identity :: Lens' a a
_identity = iso identity identity

focus :: Lens' a b -> Store r s a -> Store r s b
focus l' (Store l r) = Store (l << l') r
