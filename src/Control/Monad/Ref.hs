module Control.Monad.Ref
  ( MonadRef(..)
  , ($=)
  ) where


import Protolude

import Data.Basic
import Data.IORef



-- Interface -------------------------------------------------------------------


class Monad m => MonadRef l m | m -> l where
  ref    :: Basic a => a -> m (l a)
  deref  :: Basic a => l a -> m a
  assign :: Basic a => l a -> a -> m ()


-- Instances -------------------------------------------------------------------


instance MonadRef IORef IO where
  ref    = newIORef
  deref  = readIORef
  assign = writeIORef



-- Helpers ---------------------------------------------------------------------


infix 4 $=
($=) :: MonadRef l m => Basic a => l a -> (a -> a) -> m ()
($=) l f = do
  x <- deref l
  assign l (f x)
