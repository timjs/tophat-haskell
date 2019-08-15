module Data.Store where

import Prelude hiding (get)
import Control.Monad.Ref
import Control.Lens


-- Stores ----------------------------------------------------------------------

data Store l s t a b = Store (l s) (Lens s t a b)
type Store' l s a = Store l s s a a

store :: ( MonadRef l m ) => a -> m (Store' l a a)
store x = do
  r <- new x
  pure <| Store r value

watch :: ( MonadRef l m ) => Store' l s a -> m a
watch (Store r l) = do
  s <- read r
  pure <| get l s

assign :: ( MonadRef l m ) => a -> Store' l s a -> m ()
assign x (Store r l) = do
  r <<= set l x

focus :: Lens' a b -> Store' l s a -> Store' l s b
focus l' (Store r l) = Store r (l << l')
