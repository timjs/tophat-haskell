module Data.Share where

import Control.Monad.Ref
import Lens.Simple


-- Shares ----------------------------------------------------------------------

-- data Share r s t a b = Share (r s) (Lens s t a b)
-- type Share' r s a = Share r s s a a

-- NOTE:
-- I think the source and target types always have to be the same,
-- otherwise you would be able to change the type of the inner reference...
-- Isn't it?
data Share r s a = Share (r s) (Lens' s a)

share :: ( MonadRef r m ) => a -> m (Share r a a)
share x = do
  r <- new x
  pure <| Share r _identity

watch :: ( MonadRef r m ) => Share r s a -> m a
watch (Share r l) = do
  s <- read r
  pure <| view l s

assign :: ( MonadRef r m ) => a -> Share r s a -> m ()
assign x (Share r l) = do
  r <<= set l x


-- Focussing -------------------------------------------------------------------

_identity :: Lens' a a
_identity = iso identity identity

focus :: Lens' a b -> Share r s a -> Share r s b
focus l' (Share r l) = Share r (l << l')
