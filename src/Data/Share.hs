module Data.Share where

import Prelude hiding (get)
import Control.Monad.Ref
import Control.Lens


-- Shares ----------------------------------------------------------------------

data Share r s t a b = Share (r s) (Lens s t a b)
type Share' r s a = Share r s s a a

share :: ( MonadRef r m ) => a -> m (Share' r a a)
share x = do
  r <- new x
  pure <| Share r value

watch :: ( MonadRef r m ) => Share' r s a -> m a
watch (Share r l) = do
  s <- read r
  pure <| get l s

assign :: ( MonadRef r m ) => a -> Share' r s a -> m ()
assign x (Share r l) = do
  r <<= set l x

focus :: Lens' a b -> Share' r s a -> Share' r s b
focus l' (Share r l) = Share r (l << l')
