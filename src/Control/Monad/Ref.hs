module Control.Monad.Ref where


import Protolude

import Data.IORef



-- Interface -------------------------------------------------------------------


class Monad m => MonadRef l m | m -> l where
  new   :: a -> m (l a)
  read  :: l a -> m a
  write :: l a -> a -> m ()


infix 4 =:
(=:) :: MonadRef l m => l a -> a -> m ()
(=:) = write



-- Instances -------------------------------------------------------------------


instance MonadRef IORef IO where
  new   = newIORef
  read  = readIORef
  write = writeIORef



-- Helpers ---------------------------------------------------------------------


modify :: MonadRef l m => l a -> (a -> a) -> m ()
modify l f = do
  x <- read l
  l =: f x
